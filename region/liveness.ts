import { compilerAssert } from "../src/defs";
import { buildCFGFromRegions, ControlFlowGraph, ControlFlowGraphGeneric } from "../borrow/controlflow";
import { AccessInstruction, BasicBlock, EndAccessInstruction, FunctionBlock, GetFieldPointerInstruction, IRInstruction, LoadFromAddressInstruction, PointerOffsetInstruction, ProjectBundleInstruction, formatInstruction, getInstructionOperands, getInstructionResult } from "../borrow/defs";
import { BlockRegion, createRegionUsageMap, type InstructionId, IrDiagnostics, IrFunction, printIrFunction, Region, RegionCodegen, RegionId, Usage, UsageMap } from "./region_codegen";
import { inspect } from "bun";

type InsertMap = {[key: string]: (
  { regionId: RegionId, after: InstructionId, newInstr: IRInstruction } | 
  { regionId: RegionId, begin: true, newInstr: IRInstruction }
)[]}

type CFG = ControlFlowGraphGeneric<RegionId>

export enum LivenessType {
  LiveIn = 'LiveIn',
  LiveOut = 'LiveOut',
  LiveInAndOut = 'LiveInAndOut',
  Closed = 'Closed'
}

export class LivenessState {
  static LiveInAndOut = new LivenessState(LivenessType.LiveInAndOut);
  static LiveOut = new LivenessState(LivenessType.LiveOut);
  static LiveIn = (lastUse: InstructionId | null) => new LivenessState(LivenessType.LiveIn, lastUse);
  static Closed = (lastUse: InstructionId | null) => new LivenessState(LivenessType.Closed, lastUse);
  private constructor(public livenessType: LivenessType, public lastUse: InstructionId | null = null) { }
}

type ApproxCoverage = { isLiveIn: boolean; isLiveOut: boolean; }

// Register -> RegionId -> LivenessState
export type LivenessMap = Record<string, Record<RegionId, LivenessState>>

export const printLivenessMap = (liveness: LivenessMap) => {
  for (const [operand, livenessMap] of Object.entries(liveness)) {
    console.log(`Liveness for ${operand}:`);
    for (const [blockId, state] of Object.entries(livenessMap)) {
      console.log(`  ${blockId}: ${state.livenessType}${state.lastUse ? ` (last use: ${state.lastUse}:${state.lastUse})` : ''}`);
    }
  }
}

const getLiveness = (pass: CloseRegionAccessPass): LivenessMap => {

  const { irFunction, usage } = pass
  const cfg = buildCFGFromRegions(irFunction);
  
  const allLiveness: LivenessMap = {};

  for (const [operand_, uses] of usage) {
    const operand = operand_ as InstructionId
    
    const instr = irFunction.getInstruction(operand);
    if (instr === null) continue
    compilerAssert(instr, `No instruction found for operand ${operand}`);

    const regionId = irFunction.getInstructionRegion(operand)!
    const approximateCoverage = getApproximateCoverage(uses, regionId);

    if (Object.keys(approximateCoverage).length === 0) {
      // No uses of the operand found in the function.
      allLiveness[operand] = { [regionId]: LivenessState.Closed(lastUseOfOperand(irFunction, operand, regionId)) };
      continue
    }

    allLiveness[operand] = getFinalCoverage(approximateCoverage, operand);
  }

  return allLiveness;

  function getFinalCoverage(approximateCoverage: Record<string, ApproxCoverage>, operand: InstructionId) {
    const finalCoverage: Record<string, LivenessState> = {};
    const successors: RegionId[] = [];

    // Loop over all blocks in approximateCoverage to determine final liveness status.
    for (const [regionId_, { isLiveIn, isLiveOut }] of Object.entries(approximateCoverage)) {
      const regionId = Number(regionId_) as RegionId;

      if (isLiveIn && isLiveOut) {
        finalCoverage[regionId] = LivenessState.LiveInAndOut;
        successors.push(...cfg.successors.get(regionId)!);
      } else if (isLiveOut) {
        finalCoverage[regionId] = LivenessState.LiveOut;
        successors.push(...cfg.successors.get(regionId)!);
      } else if (isLiveIn) {
        finalCoverage[regionId] = LivenessState.LiveIn(lastUseOfOperand(irFunction, operand, regionId));
      }
    }

    // Ensure that successors of live-out blocks are marked as live-in.
    // This allows end access instructions to be inserted in unused if-else branches.
    // However I think this requires there to be an empty branch already in the CFG.
    // If not, this will mark the exit block as live-in even though it is not live-in on
    // all paths, and some paths will have two end_access instructions - which is a problem
    // for project_bundle since new actual code will be run at both end_accesses
    successors.forEach(regionId => {
      if (!finalCoverage[regionId]) {
        finalCoverage[regionId] = LivenessState.LiveIn(null); // Live in but last use unknown.
      }
    });
    return finalCoverage;
  }

  function getApproximateCoverage(uses: Usage[], regionId: RegionId) {
    let occurrences: RegionId[] = [];
    uses.forEach(use => occurrences.push(irFunction.getInstructionRegion(use.instrId)));

    const approximateCoverage: Record<string, ApproxCoverage> = {};
    while (occurrences.length > 0) {
      const occurrence = occurrences.shift()!;

      if (regionId === occurrence) continue;
      if (approximateCoverage[occurrence]?.isLiveIn) continue;

      approximateCoverage[occurrence] = { ...approximateCoverage[occurrence], isLiveIn: true };

      // const occuranceBlock = irFunction.regions[occurrence] as BlockRegion
      const predecessors = cfg.predecessors.get(occurrence) || [];
      predecessors.forEach(pred => {
        // Mark the operand as live-out at the predecessor's exit.
        approximateCoverage[pred] = { ...approximateCoverage[pred], isLiveOut: true };
        occurrences.push(pred);
      });
    }
    return approximateCoverage;
  }
};

const usesOperand = (instr: IRInstruction, operand: string): boolean => {
  return getInstructionOperands(instr).includes(operand);
}

const lastUseOfOperand = (irFunction: IrFunction, operand: string, regionid: RegionId): InstructionId => {
  const region = irFunction.regions[regionid] as BlockRegion
  compilerAssert(region, `No region found`, { regionid })
  for (let instrId = region.lastInstruction; instrId; instrId = irFunction.getInstructionNode(instrId)!.prev) {
    const instr = irFunction.getInstruction(instrId)!
    if (usesOperand(instr, operand)) {
      return instrId
    }
  }
  compilerAssert(false, `No use of operand ${operand} found in block ${regionid}`);
};



const extendLiveness = (pass: CloseRegionAccessPass, register: string) => {
  // TODO: This must be recursive to support extending from another access etc
  // In that case we may repeatedly visit the same access multiple times, so we
  // should be able to cache the extended livetime

  const { irFunction, liveness, usage } = pass
  const uses = usage.get(register);
  if (!uses) return

  for (const use of uses) {
    const instr = irFunction.getInstruction(use.instrId)!
    const dest = getInstructionResult(instr) as InstructionId
    const toExtend = instr instanceof AccessInstruction || instr instanceof LoadFromAddressInstruction || instr instanceof PointerOffsetInstruction || instr instanceof GetFieldPointerInstruction
    if (!toExtend) continue
    extendLiveness(pass, dest)
    compilerAssert(liveness[dest], `No liveness found for ${dest}`)
    mergeLivenessBlocks(irFunction, liveness[register], liveness[dest], dest)
  }
}

// Merges the liveness information from an instruction and an access instruction (as other)
const mergeLivenessBlocks = (irFunction: IrFunction, liveness: Record<string, LivenessState>, other: Record<string, LivenessState>, register: string) => {
  if (!other) return

  for (const [regionId_, state] of Object.entries(other)) {
    const regionId = Number(regionId_) as RegionId
    liveness[regionId] = getNewLiveness(regionId, state)
  }

  function getNewLiveness(regionId: RegionId, prevState: LivenessState) {

    const livenessType = liveness[regionId]?.livenessType
    const livenessTypeOther = other[regionId].livenessType

    if (!livenessType) return prevState // Just copy the state
    const thisIn = livenessType === LivenessType.LiveIn
    const thisOut = livenessType === LivenessType.LiveOut
    const thisInOut = livenessType === LivenessType.LiveInAndOut
    const thisClosed = livenessType === LivenessType.Closed
    const otherIn = livenessTypeOther === LivenessType.LiveIn
    const otherOut = livenessTypeOther === LivenessType.LiveOut
    const otherInOut = livenessTypeOther === LivenessType.LiveInAndOut
    const otherClosed = livenessTypeOther === LivenessType.Closed

    if (thisIn && otherOut || thisOut && otherIn) compilerAssert(false, 'Cannot extend live-in with live-out')
    if (thisOut || otherOut)            return LivenessState.LiveOut;
    else if (thisInOut || otherInOut)   return LivenessState.LiveInAndOut;
    else if (thisClosed && otherClosed) return LivenessState.Closed(lastUse(regionId));
    else if (thisIn && otherClosed)     return LivenessState.LiveIn(lastUse(regionId));
    
    compilerAssert(false, 'Not implemented yet', { regionId, register, liveness: liveness[regionId], other: other[regionId] })
  }

  function lastUse(blockId: RegionId) {
    const a = liveness[blockId].lastUse
    const b = other[blockId].lastUse
    // compilerAssert(false, 'Not implemented yet', { a, b })
    compilerAssert(a && b, 'No last use found')

    let n = irFunction.getInstructionNode(a)!
    while (n.next) {
      if (n.next === b) return b // b is the last use
      n = irFunction.getInstructionNode(n.next)!
    }
    return a
  }
}

export const insertRegionCloseAccesses = (pass: CloseRegionAccessPass) => {

  const { irFunction } = pass

  irFunction.regions.forEach(region => {
    if (region instanceof BlockRegion) {
      for (let instrId = region.firstInstruction; instrId; instrId = irFunction.getInstructionNode(instrId)!.next) {
        const instr = irFunction.getInstruction(instrId)
        const toClose = instr instanceof AccessInstruction || instr instanceof ProjectBundleInstruction
        if (toClose) closeAccess(pass, instr)
      }
    }
  })

  return pass
}

export class CloseRegionAccessPass {

  liveness: LivenessMap
  usage: UsageMap
  irFunction: IrFunction

  diagnostics = new IrDiagnostics()

  // Must share RegionCodegen because we need to insert instructions and we need to share the same instruction IDs
  constructor(public codegen: RegionCodegen) {
    this.irFunction = codegen.irFunction
    
  }

  insertRegionCloseAccesses() {
    this.usage = createRegionUsageMap(this.irFunction)
    this.liveness = getLiveness(this)
    insertRegionCloseAccesses(this)
  }
  
  printDebug() {
    printIrFunction(this.irFunction, this.diagnostics)
  }
  
}

const closeAccess = (pass: CloseRegionAccessPass, sourceInstr: AccessInstruction | ProjectBundleInstruction) => {
  const dest = getInstructionResult(sourceInstr)! as InstructionId
  extendLiveness(pass, dest)
  const boundaries = pass.liveness[dest]
  if (!boundaries) return

  const rootSequence = pass.irFunction.regions[pass.irFunction.getInstructionRegion(dest)!].parentSequence

  const alreadyClosed = (lastUse: InstructionId | null) => {
    if (lastUse === null) return false
    const instr = pass.irFunction.getInstruction(lastUse)
    if (instr instanceof EndAccessInstruction) {
      return instr.source === dest
    }
    return false
  }
  
  for (const [regionId_, liveness] of Object.entries(boundaries)) {
    const regionId = Number(regionId_) as RegionId

    if (liveness.livenessType === LivenessType.Closed || liveness.livenessType === LivenessType.LiveIn) {
      if (alreadyClosed(liveness.lastUse)) continue
      const newInstr = new EndAccessInstruction(dest, sourceInstr.capabilities)
      const newId = liveness.lastUse ?
        pass.codegen.insertInstructionAfter(liveness.lastUse, newInstr)
        : pass.codegen.insertInstructionAtBeginning(regionId, newInstr)
      pass.diagnostics.instructionNote(newId, `Inserted end access for ${dest} (${liveness.livenessType}) last = ${liveness.lastUse}`)
      
    }
  }

  if (sourceInstr instanceof ProjectBundleInstruction) {
    const enclosingRegions: RegionId[] = []
    for (const [regionId_, liveness] of Object.entries(boundaries)) {
      const regionId = Number(regionId_) as RegionId
      if (liveness.livenessType === LivenessType.Closed) enclosingRegions.push(regionId)
      if (liveness.livenessType === LivenessType.LiveIn) enclosingRegions.push(getEnclosingRegion(regionId))
    }
    if (enclosingRegions.length > 1) compilerAssert(enclosingRegions.every(r => r === enclosingRegions[0]), 'Multiple enclosing regions found', { dest, enclosingRegions })
    pass.diagnostics.instructionNote(dest, `Enclosing region=${enclosingRegions[0]}`)
  }

  function getEnclosingRegion(endRegionId: RegionId) {
    let regionId: RegionId | null = endRegionId
    while (regionId !== null) {
      let region: Region = pass.irFunction.regions[regionId];
      if (region.parentSequence === rootSequence) return regionId
      regionId = pass.irFunction.sequences[region.parentSequence].parentRegion
    }
    compilerAssert(false, 'No enclosing region found', { endRegionId, dest })
  }

}
