import { ControlFlowGraph } from "./controlflow";
import { AccessInstruction, BasicBlock, EndAccessInstruction, FunctionBlock, IRInstruction, InstructionId, LivenessMap, LivenessState, LivenessType, Usage, UsageMap, compilerAssert, createUsageMap, getInstructionOperands, getInstructionResult, printLivenessMap } from "./defs";

type InsertMap = {[key: string]: { at: InstructionId, newInstr: IRInstruction }[]}

const createResultMap = (blocks: BasicBlock[]) => {
  const operandToInstrMap = new Map<string, InstructionId>();
  for (const block of blocks) {
    for (let id = 0; id < block.instructions.length; id++) {
      const instr = block.instructions[id];
      const result = getInstructionResult(instr);
      if (result) {
        operandToInstrMap.set(result, new InstructionId(block.label, id));
      }
    }
  }
  return operandToInstrMap;
}


const getLiveness = (usages: UsageMap, cfg: ControlFlowGraph) => {
  
  const blocks = cfg.blocks;
  const operandToInstrMap = createResultMap(blocks);
  
  const allLiveness: LivenessMap = {};

  for (const [operand, uses] of usages) {

    // Initialize a map to store the liveness information for an individual operand.
    const approximateCoverage: Record<string, { isLiveIn: boolean, isLiveOut: boolean }> = {};

    // Initialize a set to track the blocks where the operand is used.
    let occurrences: string[] = [];
    
    const instr = operandToInstrMap.get(operand);
    if (!instr) continue
    compilerAssert(instr, `No instruction found for operand ${operand}`);
    // Collect all blocks in which the operand is being used.
    uses.forEach(use => occurrences.push(use.instrId.blockId));

    while (occurrences.length > 0) {
      const occurrence = occurrences.shift()!;

      if (instr.blockId === occurrence) continue;
      if (approximateCoverage[occurrence]?.isLiveIn) continue;

      approximateCoverage[occurrence] = { ...approximateCoverage[occurrence], isLiveIn: true };

      const occuranceBlock = blocks.find(b => b.label === occurrence)!;
      const predecessors = cfg.predecessors.get(occuranceBlock) || [];
      predecessors.forEach(pred => {
        // Mark the operand as live-out at the predecessor's exit.
        approximateCoverage[pred.label] = { ...approximateCoverage[pred.label], isLiveOut: true };
        occurrences.push(pred.label);
      });
    }

    if (Object.keys(approximateCoverage).length === 0) {
      // No uses of the operand found in the function.
      const blockId = uses[0].instrId.blockId;
      allLiveness[operand] = { [blockId]: LivenessState.Closed(lastUseOfOperand(blocks, operand, blockId)) };
      continue
    }

    const finalCoverage: Record<string, LivenessState> = {};
    const successors: BasicBlock[] = []

    // Loop over all blocks in approximateCoverage to determine final liveness status.
    for (const [blockId, { isLiveIn, isLiveOut }] of Object.entries(approximateCoverage)) {
      const block = blocks.find(b => b.label === blockId)!;

      if (isLiveIn && isLiveOut) {
        finalCoverage[blockId] = LivenessState.LiveInAndOut;
        successors.push(...cfg.successors.get(block)!);
      } else if (isLiveOut) {
        finalCoverage[blockId] = LivenessState.LiveOut;
        successors.push(...cfg.successors.get(block)!);
      } else if (isLiveIn) {
        finalCoverage[blockId] = LivenessState.LiveIn(lastUseOfOperand(blocks, operand, blockId));
      }
    }

    // Ensure that successors of live-out blocks are marked as live-in.
    successors.forEach(block => {
      if (!finalCoverage[block.label]) {
        finalCoverage[block.label] = LivenessState.LiveIn(null); // Live in but last use unknown.
      }
    });

    allLiveness[operand] = finalCoverage;
    
  }

  return allLiveness;


};

const usesOperand = (instr: IRInstruction, operand: string): boolean => {
  return getInstructionOperands(instr).includes(operand);
}

const lastUseOfOperand = (blocks: BasicBlock[], operand: string, blockId: string): InstructionId => {
  const block = blocks.find(b => b.label === blockId)!;
  for (let i = block.instructions.length - 1; i >= 0; i--) {
    const instr = block.instructions[i];
    if (usesOperand(instr, operand)) {
      return new InstructionId(blockId, i);
    }
  }
  throw new Error(`No use of operand ${operand} found in block ${blockId}`);
};



const extendLiveness = (liveness: LivenessMap, usages: UsageMap, cfg: ControlFlowGraph, register: string) => {
  // TODO: This must be recursive to support extending from another access etc
  // In that case we may repeatedly visit the same access multiple times, so we
  // should be able to cache the extended livetime
  
  const uses = usages.get(register);
  if (!uses) return
  // compilerAssert(uses, `No uses found for register ${register}`);

  for (const use of uses) {
    const block = cfg.blocks.find(b => b.label === use.instrId.blockId)!;
    const instr = block.instructions[use.instrId.instrId];
    if (instr instanceof AccessInstruction) {
      mergeLivenessBlocks(liveness[register], liveness[instr.dest], instr.dest)
    }
  }
}

// Merges the liveness information from an instruction and an access instruction (as other)
const mergeLivenessBlocks = (liveness: Record<string, LivenessState>, other: Record<string, LivenessState>, register: string) => {
  if (!other) return

  for (const [blockId, state] of Object.entries(other)) {
    
    const livenessType = liveness[blockId].livenessType
    const livenessTypeOther = other[blockId].livenessType
    compilerAssert(livenessType, `No liveness found for block ${blockId}`)

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
      compilerAssert(a && b, 'No last use found')
      return a.instrId > b.instrId ? a : b // Rely on the fact that instrId is increasing
    }
  }

}

export const insertCloseAccesses = (cfg: ControlFlowGraph, blocks: BasicBlock[], debugLog: boolean) => {

  const usage = createUsageMap(blocks)
  const liveness = getLiveness(usage, cfg)
  if (debugLog) printLivenessMap(liveness)

  const insertsMap: InsertMap = {}

  for (const block of blocks) {
    for (let i = 0; i < block.instructions.length; i++) {
      const instr = block.instructions[i]
      if (instr instanceof AccessInstruction) {
        closeAccess(cfg, liveness, usage, instr, insertsMap)
      }
    }
  }

  for (const [blockId, inserts] of Object.entries(insertsMap)) {
    // Reverse sort so we dont mess up IDs
    inserts.sort((a, b) => b.at.instrId - a.at.instrId)

    const block = blocks.find(x => x.label === blockId)!

    for (const insert of inserts) {
      block.instructions.splice(insert.at.instrId, 0, insert.newInstr)
    }
  }

}

const closeAccess = (cfg: ControlFlowGraph, liveness: LivenessMap, usage: UsageMap, sourceInstr: AccessInstruction, inserts: InsertMap) => {
  extendLiveness(liveness, usage, cfg, sourceInstr.dest)
  const boundaries = liveness[sourceInstr.dest]
  if (!boundaries) return

  const alreadyClosed = (lastUse: InstructionId | null) => {
    if (!lastUse) return false
    const block = cfg.blocks.find(b => b.label === lastUse.blockId)!
    const instr = block.instructions[lastUse.instrId]
    if (instr instanceof EndAccessInstruction) {
      return instr.source === sourceInstr.dest
    }
    return false
  }
  
  for (const [blockId, liveness] of Object.entries(boundaries)) {
    if (liveness.livenessType === LivenessType.Closed || liveness.livenessType === LivenessType.LiveIn) {
      if (alreadyClosed(liveness.lastUse)) continue
      const newInstr = new EndAccessInstruction(sourceInstr.dest, sourceInstr.capabilities)
      inserts[blockId] = inserts[blockId] || []
      const at = liveness.lastUse ? 
        new InstructionId(blockId, liveness.lastUse.instrId + 1)
        : new InstructionId(blockId, 0)
      inserts[blockId].push({ at, newInstr })
      
    }
  }

}
