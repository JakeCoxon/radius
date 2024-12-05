import { compilerAssert } from "../src/defs";
import { buildCFGFromRegions, ControlFlowGraph, ControlFlowGraphGeneric } from "../borrow/controlflow";
import { AccessInstruction, BasicBlock, EndAccessInstruction, FunctionBlock, GetFieldPointerInstruction, IRInstruction, LoadFromAddressInstruction, PointerOffsetInstruction, ProjectBundleInstruction, formatInstruction, getInstructionOperands, getInstructionResult } from "../borrow/defs";
import { BlockRegion, createRegionUsageMap, type InstructionId, IrFunction, printIrFunction, RegionCodegen, RegionId, Usage, UsageMap } from "./region_codegen";
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
  const block = irFunction.regions[regionid] as BlockRegion
  for (let instrId = block.lastInstruction; instrId; instrId = irFunction.getInstructionNode(instrId)!.prev) {
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
  // compilerAssert(uses, `No uses found for register ${register}`);

  for (const use of uses) {
    // const regionId = irFunction.getInstructionRegion(use.instrId)
    const instr = irFunction.getInstruction(use.instrId)
    // const region = irFunction.regions[regionId] as BlockRegion
    // const block = cfg.blocks.find(b => b.label === use.instrId.blockId)!;
    // const instr = block.instructions[use.instrId.instrId];
    if (instr instanceof AccessInstruction) {
      extendLiveness(pass, instr.dest)
      compilerAssert(liveness[instr.dest], `No liveness found for ${instr.dest}`)
      mergeLivenessBlocks(irFunction, liveness[register], liveness[instr.dest], instr.dest)
    } else if (instr instanceof LoadFromAddressInstruction) {
      // extendLiveness(pass, instr.dest)
      // compilerAssert(liveness[instr.dest], `No liveness found for ${instr.dest}`)
      // console.log("Extending liveness for LoadFromAddressInstruction", instr.dest, "from", register)
      // mergeLivenessBlocks(liveness[register], liveness[instr.dest], instr.dest)
    } else if (instr instanceof PointerOffsetInstruction) {
      extendLiveness(pass, instr.dest)
      compilerAssert(liveness[instr.dest], `No liveness found for ${instr.dest}`)
      // console.log("Extending liveness for PointerOffsetInstruction", instr.dest, "from", register)
      mergeLivenessBlocks(irFunction, liveness[register], liveness[instr.dest], instr.dest)
    } else if (instr instanceof GetFieldPointerInstruction) {
      extendLiveness(pass, instr.dest)
      compilerAssert(liveness[instr.dest], `No liveness found for ${instr.dest}`)
      mergeLivenessBlocks(irFunction, liveness[register], liveness[instr.dest], instr.dest)
    }

  }
}

// Merges the liveness information from an instruction and an access instruction (as other)
const mergeLivenessBlocks = (irFunction: IrFunction, liveness: Record<string, LivenessState>, other: Record<string, LivenessState>, register: string) => {
  if (!other) return

  for (const [blockId, state] of Object.entries(other)) {
    
    const livenessType = liveness[blockId]?.livenessType
    const livenessTypeOther = other[blockId].livenessType

    if (!livenessType) { // Just copy the state
      liveness[blockId] = state
      continue
    }

    if (livenessType === LivenessType.LiveIn && livenessTypeOther === LivenessType.LiveOut
      || livenessType === LivenessType.LiveOut && livenessTypeOther === LivenessType.LiveIn
    ) {
      compilerAssert(false, 'Cannot extend live-in with live-out')
    }
    if (livenessType === LivenessType.LiveOut || livenessTypeOther === LivenessType.LiveOut) {
      liveness[blockId] = LivenessState.LiveOut;
    } else if (livenessType === LivenessType.LiveInAndOut || livenessTypeOther === LivenessType.LiveInAndOut) {
      liveness[blockId] = LivenessState.LiveInAndOut;
    } else if (livenessType === LivenessType.Closed && livenessTypeOther === LivenessType.Closed) {
      liveness[blockId] = LivenessState.Closed(lastUse());
    } else if (livenessType === LivenessType.LiveIn && livenessTypeOther === LivenessType.Closed) {
      liveness[blockId] = LivenessState.LiveIn(lastUse());
    } else {
      compilerAssert(false, 'Not implemented yet', { blockId, register, liveness: liveness[blockId], other: other[blockId] })
    }
    
    function lastUse() {
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

}

export const insertRegionCloseAccesses = (pass: CloseRegionAccessPass) => {

  const insertsMap: InsertMap = {}
  pass.updateLiveness()
  const { irFunction } = pass

  irFunction.regions.forEach(region => {
    if (region instanceof BlockRegion) {
      for (let instrId = region.firstInstruction; instrId; instrId = irFunction.getInstructionNode(instrId)!.next) {
        const instr = irFunction.getInstruction(instrId)
        const toClose = instr instanceof AccessInstruction || instr instanceof ProjectBundleInstruction
        if (toClose) closeAccess(pass, instr, insertsMap)
      }
    }
  })

  return pass

}

export class CloseRegionAccessPass {

  liveness: LivenessMap
  usage: UsageMap
  irFunction: IrFunction
  debug: [InstructionId, string][] = []


  // Must share RegionCodegen because we need to insert instructions and we need to share the same instruction IDs
  constructor(public codegen: RegionCodegen) {
    this.irFunction = codegen.irFunction
    this.usage = createRegionUsageMap(this.irFunction)
  }
  updateLiveness() {
    this.liveness = getLiveness(this)
  }
  insertRegionCloseAccesses() {
    insertRegionCloseAccesses(this)
  }
  
  printDebug() {
    printIrFunction(this.irFunction, {
      instructionNotes: this.debug.reduce((acc, [id, note]) => {
        acc[id] = (acc[id] ? acc[id] + '\n' : '') + note
        return acc
      }, {} as Record<string, string>)
    })
  }
  
}

const closeAccess = (pass: CloseRegionAccessPass, sourceInstr: AccessInstruction | ProjectBundleInstruction, inserts: InsertMap) => {
  const dest = getInstructionResult(sourceInstr)! as InstructionId
  extendLiveness(pass, dest)
  const boundaries = pass.liveness[dest]
  
  if (!boundaries) return

  const alreadyClosed = (lastUse: InstructionId | null) => {
    if (lastUse === null) return false
    const instr = pass.irFunction.getInstruction(lastUse)
    if (instr instanceof EndAccessInstruction) {
      return instr.source === dest
    }
    return false
  }
  
  for (const [regionId_, liveness] of Object.entries(boundaries)) {
    if (liveness.livenessType === LivenessType.Closed || liveness.livenessType === LivenessType.LiveIn) {
      if (alreadyClosed(liveness.lastUse)) continue
      const newInstr = new EndAccessInstruction(dest, sourceInstr.capabilities)

      if (liveness.lastUse) {
        const newId = pass.codegen.insertInstructionAfter(liveness.lastUse, newInstr)
        pass.debug.push([newId, `End access for ${dest} (${liveness.livenessType}) last = ${liveness.lastUse}`])
      } else {
        compilerAssert(false, 'Not implemented yet')
      }
      
    }
  }

}
