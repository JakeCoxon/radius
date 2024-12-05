import { Capability, CapabilityRanking, compilerAssert } from "../src/defs";
import { ControlFlowGraph } from "../borrow/controlflow";
import { AccessInstruction, FunctionBlock, IRInstruction, LoadFromAddressInstruction, MoveInstruction, ProjectBundleInstruction, StoreToAddressInstruction, getInstructionResult } from "../borrow/defs";
import { BlockRegion, createRegionUsageMap, InstructionId, IrFunction } from "./region_codegen";

export class RegionReifyAccessPass {
  debugLog = false

  constructor(public irFunction: IrFunction) {
  }

  reifyAccesses() {

    const worklist: InstructionId[] = []

    const usages = createRegionUsageMap(this.irFunction)

    for (const region of this.irFunction.regions) {
      if (region instanceof BlockRegion) {
        for (let instrId = region.firstInstruction; instrId; instrId = this.irFunction.getInstructionNode(instrId).next) {
          const instr = this.irFunction.getInstruction(instrId)
          if (instr instanceof AccessInstruction) {
            worklist.push(instrId)
          } else if (instr instanceof ProjectBundleInstruction) {
            worklist.push(instrId)
          }
        }
      }
    }

    while (worklist.length > 0) {
      const instrId = worklist.shift()!
      const instr = this.irFunction.getInstruction(instrId) as AccessInstruction | ProjectBundleInstruction
      // const instr = this.cfg.blocks.find(b => b.label === instrId.blockId)!.instructions[instrId.instrId] as AccessInstruction | ProjectBundleInstruction
      const usageList = usages.get(getInstructionResult(instr)!) || []
      
      if (usageList.length === 0) {
        // Unused
        instr.capabilities = [Capability.Let]
        continue
      }

      const { min, max } = usageList.reduce((acc, usage) => {
        const usageInstr = this.irFunction.getInstruction(usage.instrId)
        const reqs = capabilitiesOfInstr(usageInstr, usage.operandIndex)
        if (reqs.length === 0) return acc
        let x = Math.min(...reqs.map(x => CapabilityRanking.indexOf(x)))
        return { min: Math.max(acc.min, x), max: Math.max(acc.max, x) }
      }, { min: 0, max: 0 })


      if (min === max) {
        const cap = instr.capabilities.find(x => CapabilityRanking.indexOf(x) >= min) ?? minCapability(instr.capabilities)
        compilerAssert(cap !== undefined, 'No capability found', { capabilities: instr.capabilities, min, max })
        instr.capabilities = [cap]
        if (instr instanceof ProjectBundleInstruction) {
          compilerAssert(instr.funcs, 'No functions found', { instr })
          compilerAssert(instr.funcs[cap] !== undefined, 'No function found', { funcs: instr.funcs, cap, instr })
        }
      } else {
        compilerAssert(false, 'Not implemented yet')
      }
    }

  }

}

const minCapability = (arr: Capability[]) => {
  let min = arr[0]
  for (const x of arr) {
    if (CapabilityRanking.indexOf(x) < CapabilityRanking.indexOf(min)) {
      min = x
    }
  }
  return min
}

const capabilitiesOfInstr = (instr: IRInstruction, operandIndex: number) => {
  if (instr instanceof AccessInstruction) {
    return instr.capabilities
  } else if (instr instanceof MoveInstruction) {
    return operandIndex === 0 ? 
      /* target */ [Capability.Inout] :  // Assume Inout vs Set for now, we don't know which until later
      /* source */ [Capability.Sink]
  } else if (instr instanceof LoadFromAddressInstruction) {
    // A load can be a let if it's a copy. We don't actually
    // use this instruction to update memory, a MarkInitialized
    // instruction is inserted afterwards. But we need to mark it here
    return [Capability.Let, Capability.Sink]
  } else if (instr instanceof StoreToAddressInstruction) {
    return [Capability.Set]
  } else return []
}