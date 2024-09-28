import { ControlFlowGraph } from "./controlflow";
import { BasicBlock, FunctionBlock, IRInstruction, InstructionId, LivenessMap, LivenessState, compilerAssert, getInstructionOperands, getInstructionResult } from "./defs";

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

export const createUsageMap = (blocks: BasicBlock[]) => {

  const usageMap = new Map<string, InstructionId[]>();
  for (const block of blocks) {
    for (let id = 0; id < block.instructions.length; id++) {
      const instr = block.instructions[id];
      const operands = getInstructionOperands(instr);
      for (const operand of operands) {
        if (usageMap.has(operand)) {
          usageMap.get(operand)!.push(new InstructionId(block.label, id));
        } else {
          usageMap.set(operand, [new InstructionId(block.label, id)]);
        }
      }
    }
  }
  return usageMap;
}

export const getLiveness = (cfg: ControlFlowGraph) => {
  
  const blocks = cfg.blocks;
  const operandToInstrMap = createResultMap(blocks);
  const usages = createUsageMap(blocks);
  const allLiveness: LivenessMap = {};

  for (const [operand, uses] of usages) {

    // Initialize a map to store the liveness information for an individual operand.
    const approximateCoverage: Record<string, { isLiveIn: boolean, isLiveOut: boolean }> = {};

    // Initialize a set to track the blocks where the operand is used.
    let occurrences: string[] = [];
    
    const instr = operandToInstrMap.get(operand);
    compilerAssert(instr, `No instruction found for operand ${operand}`);
    // Collect all blocks in which the operand is being used.
    uses.forEach(use => occurrences.push(use.blockId));

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
      const blockId = uses[0].blockId;
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
