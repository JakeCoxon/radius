import { Binding, Capability, CompiledFunction, CompilerError, GlobalCompilerState, PrimitiveType, RawPointerType, VoidType, compilerAssert } from "../src/defs"; // prettier-ignore
import { CodeGenerator } from '../borrow/codegen_ir';
import { AccessInstruction, AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, CommentInstruction, ConditionalJumpInstruction, EndAccessInstruction, FunctionBlock, GetFieldPointerInstruction, getInstructionOperands, getInstructionResult, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, MarkInitializedInstruction, Module, PhiInstruction, PhiSource, PointerOffsetInstruction, printIR, ProjectBundleInstruction, ReturnInstruction, StoreToAddressInstruction, YieldInstruction } from '../borrow/defs';
import { BlockRegion, IfRegion, InsertPosition, InstructionId, IrDiagnostics, IrFunction, printIrFunction, Region, RegionCodegen, RegionId, SequenceId, WhileRegion } from "./region_codegen";


type RegisterMapping = {
  [key: string]: string;
};

const findInstructionId = (fn: IrFunction, pred: (instr: IRInstruction) => boolean) => {
  for (const region of fn.regions) {
    if (region instanceof BlockRegion) {
      for (let instrId = region.firstInstruction; instrId !== null; instrId = fn.getInstructionNode(instrId!)!.next) {
        const instr = fn.getInstructionNode(instrId)!.instruction
        if (pred(instr)) { return instrId }
      }
    }
  }
  return null
}

const filterInstructions = (fn: IrFunction, pred: (instr: IRInstruction) => boolean) => {
  const result: InstructionId[] = []
  for (const region of fn.regions) {
    if (region instanceof BlockRegion) {
      for (let instrId = region.firstInstruction; instrId !== null; instrId = fn.getInstructionNode(instrId!)!.next) {
        const instr = fn.getInstructionNode(instrId)!.instruction
        if (pred(instr)) {
          result.push(instrId)
        }
      }
    }
  }
  return result
}

export class InlineRegionProjectBundlesPass {

  constructor(
    public globalCompiler: GlobalCompilerState,
    public codegen: CodeGenerator,
    public compiledIr: Map<Binding, IrFunction>,
  ) {}

  
  inlineRegionProjectBundlesPass() {
    this.globalCompiler.compiledFunctions.forEach((func) => {
      if (!func.body) return

      const fn = this.compiledIr.get(func.binding)!
      if (func.functionDefinition.keywords?.includes("subscript")) return

      let runs = 0
      while (true) {
        const projectInstrId = findInstructionId(fn, (instr) => instr instanceof ProjectBundleInstruction)
        if (!projectInstrId) break
        const projectInstr = fn.getInstruction(projectInstrId) as ProjectBundleInstruction
      
        const cap = projectInstr.capabilities[0]
        const binding = projectInstr.funcs[cap]
        compilerAssert(binding, "Capability not available", { projectInstr, name: func.functionDefinition.debugName })
        const subscriptIr = this.compiledIr.get(binding)!
        
        const compiledFunc = this.globalCompiler.compiledFunctions.get(func.binding)!;
        const regionCodegen = new RegionCodegen(fn, compiledFunc, this.codegen)
        const pass = new ProjectYieldInliningPass(this.globalCompiler, regionCodegen, compiledFunc, fn, subscriptIr, runs)

        try {
          pass.inlineIr(fn, projectInstrId, subscriptIr)
        } catch (e) {
          if (e instanceof CompilerError) {
            if ((e.info as any).instrId) pass.diagnostics.instructionNote((e.info as any).instrId, e.message)
          }
          pass.printDebug()
          throw e
        }
        runs ++
      }

      if (runs) {
        console.log(`Inlined ${runs} project bundles in ${func.functionDefinition.debugName}`)
        printIrFunction(fn)
      }
    })
  }
}

class ProjectYieldInliningPass {
  diagnostics: IrDiagnostics = new IrDiagnostics()
  
  mapping: RegisterMapping = {}

  constructor(
    public globalCompiler: GlobalCompilerState,
    public codegen: RegionCodegen,
    public compiledFunction: CompiledFunction,
    public fn: IrFunction,
    public subscriptIr: IrFunction,
    public runId: number
  ) {}

  printDebug() {
    printIrFunction(this.fn, this.diagnostics)
  }

  inlineIr(fn: IrFunction, instrId: InstructionId, newIr: IrFunction) {
    const projectInstr = fn.getInstruction(instrId) as ProjectBundleInstruction
    const originalRegionId = fn.getInstructionRegion(instrId)
    const originalSequenceId = fn.regions[originalRegionId].parentSequence

    const [prev, next] = this.codegen.splitBlockBeforeInstr(instrId)
    this.codegen.setInsertionBlock(prev)
    // prev.instructions.pop() // Remove the jump instruction, we'll replace it
    this.codegen.insertInstruction(new CommentInstruction(`Project bundle inlining parameters`))
    const inserts = this.codegen.insertInstructionsAtPosition(
      InsertPosition.endOfRegion(prev), 
      this.createParameterInstructions(newIr, projectInstr))
    this.diagnostics.instructionNote(inserts.lastId!, "Inserted inlining parameters")

    this.codegen.insertInstructionAfter(instrId, new CommentInstruction(`Deleted project bundle (${instrId})`))
    this.codegen.deleteInstruction(instrId)


    printIrFunction(newIr)

    const newRegionsInserts = copyRegionsAfterRegion(this.codegen, this.mapping, fn, newIr, prev)

    // Handle returns
    const onePastEndRegionId = fn.regions[newRegionsInserts.endRegionId!].nextRegion
    for (let regionId = newRegionsInserts.startRegionId; regionId !== null && regionId !== onePastEndRegionId; regionId = fn.regions[regionId].nextRegion) {
      const region = fn.regions[regionId]
      if (region instanceof BlockRegion) {
        for (let instrId = region.firstInstruction; instrId !== null; instrId = fn.getInstructionNode(instrId)?.next ?? null) {
          const instr = fn.getInstructionNode(instrId)!.instruction
          if (instr instanceof ReturnInstruction) {
            if (regionId === newRegionsInserts.endRegionId && fn.getInstructionNode(instrId)!.next === null) {
              this.codegen.deleteInstruction(instrId)
            } else {
              compilerAssert(false, "Unsupported return instruction", { instr, instrId, regionId, startRegionId: newRegionsInserts.startRegionId, endRegionId: newRegionsInserts.endRegionId })
            }
          }
        }
      }
    }
    

    const yieldInstrs = filterInstructions(fn, (instr) => instr instanceof YieldInstruction)
    compilerAssert(yieldInstrs.length > 0, "Yield instruction not found", { instrId })
    compilerAssert(yieldInstrs.length === 1, "Multiple yields not supported", { instrId })
    // const yieldInstrId = this.mapping[yieldInstrs[0]] as InstructionId
    const yieldInstrId = yieldInstrs[0]
    const found = filterInstructions(fn, (instr) => instr instanceof YieldInstruction);
    // compilerAssert(found, "Yield instruction not found", { instrId, yieldInstrId })
    compilerAssert(found.includes(yieldInstrId), "Not found yield instr", { yieldInstrs, found, mapped: yieldInstrs.map(f => this.mapping[f]) })

    const yieldSeq = fn.regions[fn.getInstructionRegion(yieldInstrId)].parentSequence
    compilerAssert(yieldSeq === originalSequenceId, "Yield not in the root sequence. Not implemented yet", { yieldInstrId, yieldSeq, originalSequenceId }) // TODO: Will need to consider how to handle this with multiple end instructions

    const yieldAtEnd = fn.getInstructionRegion(yieldInstrId) === newRegionsInserts.endRegionId
    const [yieldPrev, yieldNext] = this.codegen.splitBlockBeforeInstr(yieldInstrId)
    this.codegen.insertInstructionAtBeginning(yieldNext, new CommentInstruction(`!! After split copied region`))
    if (yieldAtEnd) newRegionsInserts.endRegionId = yieldNext

    {
      const yieldInstr = fn.getInstruction(yieldInstrId) as YieldInstruction
      // compilerAssert(yieldInstr.type instanceof PrimitiveType, "Yield type non-primitive not supported yet", { yieldInstr })
      compilerAssert(yieldInstr.value, "Yield value not found. not implemented", { yieldInstr })
      // const newDest = this.codegen.newRegister()
      // Assign is aliasing the value, so it is okay for non-primitive types
      const newInstr = new AssignInstruction(projectInstr.target, yieldInstr.type, yieldInstr.value)
      const region = fn.regions[yieldPrev] as BlockRegion
      this.codegen.insertInstructionAfter(region.lastInstruction!, newInstr)

    }
    this.codegen.deleteInstruction(yieldInstrId)
    // this.diagnostics.instructionNote(yieldInstrId, "To be deleted")

    const endInstrs = filterInstructions(fn, (instr) => instr instanceof EndAccessInstruction && instr.source === projectInstr.target)
    compilerAssert(endInstrs.length > 0, "End instruction not found", { instrId })

    const moveRange = (() => {


      if (endInstrs.length === 1) {
        
        // If there is only one end instruction, we can split the region at the end instruction
        // position and move the first part of the region inside the new function

        const endInstrId = endInstrs[0]
        compilerAssert(endInstrId, "End instruction not found", { instrId, fn })

        const endRegionId = fn.getInstructionRegion(endInstrId);
        const endSequenceId = fn.regions[endRegionId].parentSequence
        if (endSequenceId === originalSequenceId) {
          // Easy case, the end instruction is in the same sequence

          const [endPrev, endNext] = this.codegen.splitBlockBeforeInstr(endInstrId)
          
          this.diagnostics.instructionNote(endInstrId, "To be deleted")

           // Move the regions after the yield to after the end instruction
          this.codegen.moveRegionsToAfter(endPrev, yieldNext, newRegionsInserts.endRegionId!)

          return
        }
      }

      // If there are multiple end instructions  or the end instruction is in a different sequence,
      // we need to move the regions to the right place. In the future we can try to use an interleave region

      const enclosingRegions = endInstrs.map(endInstrId => getEnclosingRegion(instrId, endInstrId))
      compilerAssert(enclosingRegions.every((r) => r === enclosingRegions[0]), "Not all enclosing regions are the same", { enclosingRegions })
      const enclosingRegion = enclosingRegions[0]

      // compilerAssert(false, "Not implemented yet", { next, endInstrs, enclosingRegion, newRegionsInserts, yieldPrev, yieldNext })
      this.codegen.moveRegionsToAfter(yieldPrev, next, enclosingRegion)

      
    })()

    function getEnclosingRegion(startBundleId: InstructionId, instr: InstructionId) {
      const rootSequence = fn.regions[fn.getInstructionRegion(startBundleId)].parentSequence
      const endRegionId = fn.getInstructionRegion(instr)
      let regionId: RegionId | null = endRegionId
      while (regionId !== null) {
        let region: Region = fn.regions[regionId];
        if (region.parentSequence === rootSequence) return regionId
        regionId = fn.sequences[region.parentSequence].parentRegion
      }
      compilerAssert(false, 'No enclosing region found', { endRegionId, instr })
    }
  }

  createParameterInstructions(newIr: IrFunction, projectInstr: ProjectBundleInstruction) {
    const insertedInstructions: IRInstruction[] = []
    newIr.parameterRegisters.forEach((reg, i) => {
      this.mapping[reg] = this.codegen.newRegister()
      const passingType = newIr.params[i].passingType
      const source = i === 0 ? projectInstr.source : projectInstr.operands[i - 1]

      // @ParameterPassing
      if (passingType === RawPointerType) {
        insertedInstructions.push(new AssignInstruction(this.mapping[reg], passingType, source))
        return
      }
      insertedInstructions.push(new AllocInstruction(this.mapping[reg], passingType))
      insertedInstructions.push(new StoreToAddressInstruction(this.mapping[reg], passingType, source))
    })
    return insertedInstructions
  }

}

const copyRegionsAfterRegion = (
  codegen: RegionCodegen, mapping: RegisterMapping, destFn: IrFunction, sourceFn: IrFunction, prevRegionId: RegionId) => {

  const existingSequence = destFn.regions[prevRegionId].parentSequence

  const traverseSequence = (destParentSequenceId: SequenceId, prevRegionId: RegionId | null, sequenceId: SequenceId) => {
    const seq = sourceFn.sequences[sequenceId]

    const inserts = { startRegionId: null as RegionId | null, endRegionId: null as RegionId | null }

    const insert = (newRegionId: RegionId) => {
      if (prevRegionId === null) codegen.insertSequenceChildAtBeginning(destParentSequenceId, newRegionId)
      else codegen.insertSequenceChildAfter(destParentSequenceId, prevRegionId, newRegionId)
      if (!inserts.startRegionId) inserts.startRegionId = newRegionId
      inserts.endRegionId = newRegionId
    }

    
    for (let regionId = seq.firstChildRegion; regionId !== null; regionId = sourceFn.regions[regionId].nextRegion) {
      const region = sourceFn.regions[regionId]
      if (region instanceof BlockRegion) {
        const newRegionId = codegen.insertNewBlockRegion()
        insert(newRegionId)
        prevRegionId = newRegionId
        codegen.setInsertionBlock(newRegionId)
        codegen.insertInstruction(new CommentInstruction(`!! Copied region`))
        mapRegionInstructions(codegen, sourceFn, mapping, region, (instr) => {
          codegen.insertInstruction(instr)
        })
      } else if (region instanceof IfRegion) {
        const newRegionId = codegen.insertNewIfRegion()
        const newRegion = codegen.getIfRegion(newRegionId)
        insert(newRegionId)
        prevRegionId = newRegionId
        traverseSequence(newRegion.conditionSequence, null, region.conditionSequence)
        traverseSequence(newRegion.thenSequence, null, region.thenSequence)
        traverseSequence(newRegion.elseSequence, null, region.elseSequence)
        traverseSequence(newRegion.exitSequence, null, region.exitSequence)
        newRegion.conditionRegister = mapping[region.conditionRegister]
        newRegion.result = mapping[region.result]
        compilerAssert(newRegion.conditionRegister !== undefined, "Condition register not found", { 
          irFunction: sourceFn.debugName,
          newRegionId, regionId,
          region, newRegion, mapping })
      } else if (region instanceof WhileRegion) {
        const newRegionId = codegen.insertNewWhileRegion()
        const newRegion = codegen.getWhileRegion(newRegionId)
        insert(newRegionId)
        prevRegionId = newRegionId
        traverseSequence(newRegion.conditionSequence, null, region.conditionSequence)
        traverseSequence(newRegion.bodySequence, null, region.bodySequence)
        newRegion.conditionRegister = mapping[region.conditionRegister]
        compilerAssert(newRegion.conditionRegister !== undefined, "Condition register not found", { region, newRegion })
      }
    }
    return inserts
  }
      
  return traverseSequence(existingSequence, prevRegionId, sourceFn.root)

}


const mapRegionInstructions = (codegen: RegionCodegen, fn: IrFunction, mapping: RegisterMapping, region: BlockRegion, func: (instr: IRInstruction) => void) => {
  const newRegister = (prev: string) => {
    const reg = codegen.newRegister()
    mapping[prev] = reg
    return reg
  }
  
  for (let instrId = region.firstInstruction; instrId !== null; instrId = fn.getInstructionNode(instrId)!.next) {
    const instr = fn.getInstructionNode(instrId)!.instruction

    if (instr instanceof LoadConstantInstruction) {
      const newDest = newRegister(instr.dest)
      const load = new LoadConstantInstruction(newDest, instr.type, instr.value)
      func(load)
    } else if (instr instanceof ReturnInstruction) {
      const newSource = instr.value ? mapping[instr.value] : null
      const newInstr = new ReturnInstruction(instr.type, newSource)
      func(newInstr)
    } else if (instr instanceof ProjectBundleInstruction) {
      const newTarget = newRegister(instr.target)
      const newSource = mapping[instr.source]
      const newOperands = instr.operands.map(op => mapping[op])
      const newInstr = new ProjectBundleInstruction(newTarget, instr.type, instr.capabilities, newSource, newOperands, instr.funcs)
      func(newInstr)
    } else if (instr instanceof PointerOffsetInstruction) {
      const newDest = newRegister(instr.dest)
      const newAddress = mapping[instr.address]
      const newOffsetReg = mapping[instr.offsetReg]
      const newInstr = new PointerOffsetInstruction(newDest, newAddress, instr.fieldType, newOffsetReg)
      func(newInstr)
    } else if (instr instanceof AllocInstruction) {
      const newDest = newRegister(instr.dest)
      const newInstr = new AllocInstruction(newDest, instr.type)
      func(newInstr)
    } else if (instr instanceof AccessInstruction) {
      const newDest = newRegister(instr.dest)
      const newSource = mapping[instr.source]
      const newInstr = new AccessInstruction(newDest, newSource, instr.capabilities, instr.type)
      func(newInstr)
    } else if (instr instanceof EndAccessInstruction) {
      const newSource = mapping[instr.source]
      const newInstr = new EndAccessInstruction(newSource, instr.capabilities)
      func(newInstr)
    } else if (instr instanceof StoreToAddressInstruction) {
      const newSource = mapping[instr.source]
      const newAddress = mapping[instr.address]
      const newInstr = new StoreToAddressInstruction(newAddress, instr.type, newSource)
      func(newInstr)
    } else if (instr instanceof LoadFromAddressInstruction) {
      const newDest = newRegister(instr.dest)
      const newAddress = mapping[instr.address]
      const newInstr = new LoadFromAddressInstruction(newDest, instr.type, newAddress)
      func(newInstr)
    } else if (instr instanceof YieldInstruction) {
      const newDest = newRegister(instr.dest)
      const newValue = instr.value ? mapping[instr.value] : null
      const newInstr = new YieldInstruction(newDest, instr.type, newValue)
      func(newInstr)
    } else if (instr instanceof MarkInitializedInstruction) {
      const newTarget = mapping[instr.target]
      const newInstr = new MarkInitializedInstruction(newTarget, instr.type, instr.initialized)
      func(newInstr)
    } else if (instr instanceof BinaryOperationInstruction) {
      const newDest = newRegister(instr.dest)
      const newLeft = mapping[instr.left]
      const newRight = mapping[instr.right]
      const newInstr = new BinaryOperationInstruction(newDest, instr.type, instr.operator, newLeft, newRight, instr.paramType)
      func(newInstr)
    } else if (instr instanceof GetFieldPointerInstruction) {
      const newDest = newRegister(instr.dest)
      const newAddress = mapping[instr.address]
      const newInstr = new GetFieldPointerInstruction(newDest, newAddress, instr.field)
      func(newInstr)
    } else if (instr instanceof ConditionalJumpInstruction) {
      const newCondition = mapping[instr.condition]
      const newTarget = mapping[instr.targetLabel]
      const newElse = mapping[instr.elseLabel]
      const newInstr = new ConditionalJumpInstruction(newCondition, newTarget, newElse)
      func(newInstr)
    } else if (instr instanceof JumpInstruction) {
      const newTarget = mapping[instr.target]
      const newInstr = new JumpInstruction(newTarget)
      func(newInstr)
    } else if (instr instanceof CallInstruction) {
      const newTarget = instr.target ? newRegister(instr.target) : null
      const newArgs = instr.args.map(arg => mapping[arg])
      const newInstr = new CallInstruction(newTarget, instr.type, instr.binding, newArgs, instr.paramTypes, instr.capabilities)
      func(newInstr)
    } else if (instr instanceof PhiInstruction) {
      const newDest = newRegister(instr.dest)
      const newSources = instr.sources.map(val => new PhiSource(mapping[val.value], mapping[val.block]))
      const newInstr = new PhiInstruction(newDest, instr.type, newSources)
      func(newInstr)
    } else if (instr instanceof CommentInstruction) {
      const newInstr = new CommentInstruction(instr.comment)
      func(newInstr)
    } else compilerAssert(false, "Instruction not found", { instr })
  }

}
