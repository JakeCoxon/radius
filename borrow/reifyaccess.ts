import { ControlFlowGraph } from "./controlflow";
import { AccessInstruction, Capability, FunctionBlock, IRInstruction, InstructionId, LoadFromAddressInstruction, MoveInstruction, StoreToAddressInstruction, compilerAssert, createUsageMap } from "./defs";

const CapabilityRanking = [Capability.Let, Capability.Set, Capability.Inout, Capability.Sink]

export class ReifyAccessPass {
  debugLog = false

  constructor(public cfg: ControlFlowGraph) {
  }

  reifyAccesses() {

    const worklist: InstructionId[] = []

    const usages = createUsageMap(this.cfg.blocks)

    for (const block of this.cfg.blocks) {
      for (let i = 0; i < block.instructions.length; i++) {
        const instr = block.instructions[i]
        if (instr instanceof AccessInstruction) {

          // if (instr.capabilities.length === 1) continue

          worklist.push(new InstructionId(block.label, i))
        }
      }
    }

    while (worklist.length > 0) {
      const instrId = worklist.pop()!
      const instr = this.cfg.blocks.find(b => b.label === instrId.blockId)!.instructions[instrId.instrId] as AccessInstruction
      const usageList = usages.get(instr.dest) || []
      compilerAssert(usageList.length > 0, 'No usage instrs found')

      const { min, max } = usageList.reduce((acc, usage) => {
        const instr = this.cfg.blocks.find(b => b.label === usage.instrId.blockId)!.instructions[usage.instrId.instrId]
        const reqs = capabilitiesOfInstr(instr, usage.operandIndex)
        if (reqs.length === 0) return acc
        let x = Math.min(...reqs.map(x => CapabilityRanking.indexOf(x)))
        return { min: Math.max(acc.min, x), max: Math.max(acc.max, x) }
      }, { min: 0, max: 0 })

      if (min === max) {
        const cap = instr.capabilities.find(x => CapabilityRanking.indexOf(x) >= min)
        compilerAssert(cap !== undefined, 'No capability found')
        instr.capabilities = [cap]
      } else {
        compilerAssert(false, 'Not implemented yet')
      }
    }

  }

}

const capabilitiesOfInstr = (instr: IRInstruction, operandIndex: number) => {
  if (instr instanceof AccessInstruction) {
    return instr.capabilities
  } else if (instr instanceof MoveInstruction) {
    return operandIndex === 0 ? 
      /* target */ [Capability.Inout] :  // Assume Inout vs Set for now, we don't know which until later
      /* source */ [Capability.Sink]
  } else if (instr instanceof LoadFromAddressInstruction) {
    return [Capability.Sink]
  } else if (instr instanceof StoreToAddressInstruction) {
    return [Capability.Set]
  } else return []
}