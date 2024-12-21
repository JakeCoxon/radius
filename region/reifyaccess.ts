import { Capability, CapabilityRanking, compilerAssert } from "../src/defs";
import { ControlFlowGraph } from "../borrow/controlflow";
import { AccessInstruction, FunctionBlock, IRInstruction, LoadFromAddressInstruction, MoveInstruction, ProjectBundleInstruction, StoreToAddressInstruction, getInstructionResult } from "../borrow/defs";
import { BlockRegion, createRegionUsageMap, InstructionId, IrDiagnostics, IrFunction, printIrFunction } from "./region_codegen";

export class RegionReifyAccessPass {
  debugLog = false
  diagnostics = new IrDiagnostics()

  constructor(public irFunction: IrFunction) {
  }

  printDebug() {
    printIrFunction(this.irFunction, this.diagnostics)
  }

  reifyAccesses() {
    try {
      this.uncheckedReifyAccesses()
    } catch (e) {
      this.printDebug()
      throw e
    }
  }

  uncheckedReifyAccesses() {

    const worklist: InstructionId[] = []

    const usages = createRegionUsageMap(this.irFunction)
    let iterationIndex = 0

    for (const region of this.irFunction.regions) {
      if (region instanceof BlockRegion) {
        for (let instrId = region.firstInstruction; instrId !== null; instrId = this.irFunction.getInstructionNode(instrId)!.next) {
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
      if (iterationIndex++ > 10000) compilerAssert(false, `Potential infinite loop. Shouldn't ever happen`)

      const instrId = worklist.shift()!
      const instr = this.irFunction.getInstruction(instrId) as AccessInstruction | ProjectBundleInstruction
      const usageList = usages.get(getInstructionResult(instr)!) || []
      
      if (usageList.length === 0) {
        // Unused
        instr.capabilities = [Capability.Let]
        continue
      }


      const { min, max } = usageList.reduce((acc, usage) => {
        const usageInstr = this.irFunction.getInstruction(usage.instrId)!
        const reqs = capabilitiesOfInstr(usageInstr, usage.operandIndex)
        if (reqs.length === 0) return acc
        const reqRanks = reqs.map(x => CapabilityRanking.indexOf(x));
        let minReqs = Math.min(...reqRanks)

        this.diagnostics.instructionNote(instrId, `Usage: ${usage.instrId} ${usage.operandIndex} ${reqs.join(', ')}`)

        return { min: Math.max(acc.min, minReqs), max: Math.max(acc.max, ...reqRanks) }
      }, { min: 0, max: 0 })



      if (min !== max) {
        worklist.push(instrId)
        continue
      }

      let cap = instr.capabilities.find(x => CapabilityRanking.indexOf(x) >= min)
      if (!cap) {
        // TODO: Need a test case for this. Why would it happen?
        compilerAssert(instr.capabilities.length === 1, 'Multiple capabilities found', { instr })
        cap = instr.capabilities[0]
      }
      
      compilerAssert(cap !== undefined, 'No capability found', { capabilities: instr.capabilities, min, max })
      instr.capabilities = [cap]
      if (instr instanceof ProjectBundleInstruction) {
        compilerAssert(instr.funcs, 'No functions found', { instr })
        compilerAssert(instr.funcs[cap] !== undefined, 'No function found', { funcs: instr.funcs, cap, instr })
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
  } else if (instr instanceof ProjectBundleInstruction) {
    return instr.capabilities
  } else if (instr instanceof MoveInstruction) {
    return operandIndex === 0 ? 
      /* target */ [Capability.Inout] :  // Assume Inout vs Set for now, we don't know which until later
      /* source */ [Capability.Sink]
  } else if (instr instanceof LoadFromAddressInstruction) {
    // A load can be a let if it's a copy. We don't actually
    // use this instruction to update memory, a MarkInitialized
    // instruction is inserted afterwards. But we need to mark it here
    // return [Capability.Let, Capability.Sink]

    // This is basically just to signify to use the maximum capability
    // of the access instruction. It does not mean that the instruction 
    // will actually sink a value
    return [Capability.Sink]
  } else if (instr instanceof StoreToAddressInstruction) {
    // This is a bit of a hack. I think it should use an access instruction
    // instead of the store instruction referring to the register directly.
    // I think I made a mistake somewhere. Needs some test cases.
    return operandIndex === 0 ?
      /* target */ [Capability.Set] :
      /* source */ [Capability.Let]
  } else return []
}