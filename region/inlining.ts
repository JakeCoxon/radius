import { Binding, Capability, CompiledFunction, GlobalCompilerState, PrimitiveType, RawPointerType, VoidType, compilerAssert } from "../src/defs"; // prettier-ignore
import { CodeGenerator } from '../borrow/codegen_ir';
import { AccessInstruction, AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, CommentInstruction, ConditionalJumpInstruction, EndAccessInstruction, FunctionBlock, GetFieldPointerInstruction, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, MarkInitializedInstruction, Module, PhiInstruction, PhiSource, PointerOffsetInstruction, printIR, ProjectBundleInstruction, ReturnInstruction, StoreToAddressInstruction, YieldInstruction } from '../borrow/defs';
import { BlockRegion, IfRegion, InsertPosition, InstructionId, IrDiagnostics, IrFunction, printIrFunction, Region, RegionCodegen, RegionId, SequenceId, WhileRegion } from "./region_codegen";

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

// const filterInstructions = (fn: FunctionBlock, pred: (instr: IRInstruction) => boolean) => {
//   const result: InstructionId[] = []
//   for (const block of fn.blocks) {
//     for (let i = 0; i < block.instructions.length; i++) {
//       const instr = block.instructions[i]
//       if (pred(instr)) {
//         result.push(new InstructionId(block.label, i))
//       }
//     }
//   }
//   return result
// }

// const getInstructionById = (fn: FunctionBlock, instrId: InstructionId) => {
//   const block = fn.blocks.find(b => b.label === instrId.blockId)!
//   return block.instructions[instrId.instrId]
// }

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
        const returningTarget = projectInstr.target
        
        const compiledFunc = this.globalCompiler.compiledFunctions.get(func.binding)!;
        const regionCodegen = new RegionCodegen(fn, compiledFunc, this.codegen)
        const pass = new ProjectYieldInliningPass(this.globalCompiler, regionCodegen, compiledFunc, fn, subscriptIr)

        try {
          pass.inlineIr(fn, projectInstrId, subscriptIr, returningTarget)
        } catch (e) {
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
  ) {}

  printDebug() {
    printIrFunction(this.fn, this.diagnostics)
  }

  inlineIr(fn: IrFunction, instrId: InstructionId, newIr: IrFunction, returningTarget: string) {
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

    this.codegen.deleteInstruction(instrId)
    // this.diagnostics.instructionNote(instrId, "To be deleted")

    // prev.instructions.push(new CommentInstruction(`Project bundle inlining parameters`))
    // prev.instructions.push(...this.createParameterInstructions(newIr, projectInstr, mapping))
    // next.instructions.shift() // Remove the project instruction


    const endInstrs = filterInstructions(fn, (instr) => instr instanceof EndAccessInstruction && instr.source === projectInstr.target)
    compilerAssert(endInstrs.length > 0, "End instruction not found", { instrId })
    compilerAssert(endInstrs.length === 1, "Multiple end instructions not supported yet", { instrId })

    const endInstrId = endInstrs[0]
    compilerAssert(endInstrId, "End instruction not found", { instrId, fn })

    const endRegionId = fn.getInstructionRegion(endInstrId);
    const endSequenceId = fn.regions[endRegionId].parentSequence
    compilerAssert(endSequenceId === originalSequenceId, "End sequence not the same. Not implemented yet", { endSequenceId, originalSequenceId })

    const [endPrev, endNext] = this.codegen.splitBlockBeforeInstr(endInstrId)

    this.diagnostics.instructionNote(endInstrId, "To be deleted")

    printIrFunction(newIr)

    copyRegionsAfterRegion(this.codegen, this.mapping, fn, newIr, prev)

    const yieldInstrs = filterInstructions(fn, (instr) => instr instanceof YieldInstruction)
    compilerAssert(yieldInstrs.length > 0, "Yield instruction not found", { instrId })
    compilerAssert(yieldInstrs.length === 1, "Multiple yields not supported", { instrId })
    // const yieldInstrId = this.mapping[yieldInstrs[0]] as InstructionId
    const yieldInstrId = yieldInstrs[0]
    const found = filterInstructions(fn, (instr) => instr instanceof YieldInstruction);
    // compilerAssert(found, "Yield instruction not found", { instrId, yieldInstrId })
    compilerAssert(found.includes(yieldInstrId), "Not found yield instr", { yieldInstrs, found, mapped: yieldInstrs.map(f => this.mapping[f]) })

    const [yieldPrev, yieldNext] = this.codegen.splitBlockBeforeInstr(yieldInstrId)
    

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

    this.codegen.moveRegionsToAfter(yieldPrev, endPrev, endPrev)

    // compilerAssert(false, "Not implemented", { prev, next })

    // const returningBlockLabel = endNext.label
    // const yieldBlockLabel = endPrev.label
    // const state: CopyBlockState = { mapping, returningBlockLabel, returningTarget, yieldBlockLabel, yieldTarget: projectInstr.target }
    // this.insertCopiedBlocks(mapping, fn, instrId, newIr, state)


    // const newEntry = mapping[newIr.blocks[0].label]
    // compilerAssert(newEntry, "Entry not found", { newIr, mapping })
    // prev.instructions.push(new JumpInstruction(newEntry))
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

    const insert = (newRegionId: RegionId) => {
      if (prevRegionId === null) codegen.insertSequenceChildAtBeginning(destParentSequenceId, newRegionId)
      else codegen.insertSequenceChildAfter(destParentSequenceId, prevRegionId, newRegionId)
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
      } else if (region instanceof WhileRegion) {
        const newRegionId = codegen.insertNewWhileRegion()
        insert(newRegionId)
        prevRegionId = newRegionId
      }
    }
  }
      
  traverseSequence(existingSequence, prevRegionId, sourceFn.root)

}



type RegisterMapping = {
  [key: string]: string;
};
class CopyBlockState {
  mapping: RegisterMapping;
  constructor(
    public codegen: CodeGenerator,
    public fn: IrFunction,
    public returningBlockLabel: string,
    public returningTarget: string,
    public yieldBlockLabel: string,
    public yieldTarget: string,
  ) {
    this.mapping = {};
  }

  insertCopiedBlocks(newIr: IrFunction, instrId: InstructionId) {

    // copyRegions(
    // newIr.regions.forEach((b) => {
    //   this.mapping[b.label] = this.codegen.newLabel();
    // });

    // const newBlocks: BasicBlock[] = [];
    // let currentBlock: BasicBlock;
    // let yieldReturnBlockLabel = "";
    // newIr.blocks.forEach((b) => {
    //   const label = this.mapping[b.label];
    //   currentBlock = new BasicBlock(label, []);
    //   newBlocks.push(currentBlock);
    //   mapInstructions(this.codegen, this.mapping, b.instructions, (instr) => {
    //     if (instr instanceof YieldInstruction) {
    //       compilerAssert(!yieldReturnBlockLabel, "Multiple yields not supported", { instr });
    //       currentBlock.instructions.push(new CommentInstruction(`Inlining yield ${instr.value} -> ${this.yieldTarget}`));
    //       if (instr.value) {
    //         currentBlock.instructions.push(new AssignInstruction(this.yieldTarget, instr.type, instr.value));
    //         // TODO: Do we need to store the value sometimes? or is it always assign?
    //         // currentBlock.instructions.push(new AllocInstruction(copyState.yieldTarget, instr.type))
    //         // currentBlock.instructions.push(new StoreToAddressInstruction(copyState.yieldTarget, instr.type, instr.value))
    //       }
    //       currentBlock.instructions.push(new JumpInstruction(this.yieldBlockLabel));
    //       currentBlock = new BasicBlock(this.codegen.newLabel(), []);
    //       yieldReturnBlockLabel = currentBlock.label;

    //       const yieldFrom = this.fn.blocks.find((b) => b.label === this.yieldBlockLabel)!;
    //       yieldFrom.instructions.pop();
    //       yieldFrom.instructions.push(new JumpInstruction(yieldReturnBlockLabel));

    //       newBlocks.push(currentBlock)
    //       return
    //     } else if (instr instanceof ReturnInstruction) {
    //       // TODO: Support returning better by rewriting it as a block and break in the AST stage
    //       if (!instr.value) {
    //         return void currentBlock.instructions.push(new JumpInstruction(this.returningBlockLabel))
    //       }
    //       currentBlock.instructions.push(new AllocInstruction(this.returningTarget, instr.type))
    //       currentBlock.instructions.push(new StoreToAddressInstruction(this.returningTarget, instr.type, instr.value))
    //       const jump = new JumpInstruction(this.returningBlockLabel)
    //       currentBlock.instructions.push(jump)
    //       return
    //     } else if (instr instanceof LoadFromAddressInstruction) {
    //       compilerAssert(instr.type !== VoidType, "Cannot load void type", { instr })
    //     }
    //     currentBlock.instructions.push(instr)
    //   })
    // })

    // compilerAssert(yieldReturnBlockLabel, "No yield found")

    // const index = this.fn.blocks.findIndex(b => b.label === instrId.blockId)!
    // compilerAssert(index >= 0, "Block not found", { instrId, fn })
    // this.fn.blocks.splice(index + 1, 0, ...newBlocks)
  }

};
// const insertCopiedBlocks = (codegen: CodeGenerator, mapping: RegisterMapping, fn: FunctionBlock, instrId: InstructionId, newIr: FunctionBlock, copyState: CopyBlockState) => {

//   newIr.blocks.forEach(b => {
//     mapping[b.label] = codegen.newLabel()
//   })

//   const newBlocks: BasicBlock[] = []
//   let currentBlock: BasicBlock
//   let yieldReturnBlockLabel = ""
//   newIr.blocks.forEach(b => {
//     const label = mapping[b.label]
//     currentBlock = new BasicBlock(label, [])
//     newBlocks.push(currentBlock)
//     mapInstructions(codegen, mapping, b.instructions, (instr) => {
//       if (instr instanceof YieldInstruction) {
//         compilerAssert(!yieldReturnBlockLabel, "Multiple yields not supported", { instr, copyState })
//         currentBlock.instructions.push(new CommentInstruction(`Inlining yield ${instr.value} -> ${copyState.yieldTarget}`))
//         if (instr.value) {
//           currentBlock.instructions.push(new AssignInstruction(copyState.yieldTarget, instr.type, instr.value))
//           // TODO: Do we need to store the value sometimes? or is it always assign?
//           // currentBlock.instructions.push(new AllocInstruction(copyState.yieldTarget, instr.type))
//           // currentBlock.instructions.push(new StoreToAddressInstruction(copyState.yieldTarget, instr.type, instr.value))
//         }
//         currentBlock.instructions.push(new JumpInstruction(copyState.yieldBlockLabel))
//         currentBlock = new BasicBlock(codegen.newLabel(), [])
//         yieldReturnBlockLabel = currentBlock.label

//         const yieldFrom = fn.blocks.find(b => b.label === copyState.yieldBlockLabel)!
//         yieldFrom.instructions.pop()
//         yieldFrom.instructions.push(new JumpInstruction(yieldReturnBlockLabel))

//         newBlocks.push(currentBlock)
//         return
//       } else if (instr instanceof ReturnInstruction) {
//         // TODO: Support returning better by rewriting it as a block and break in the AST stage
//         if (!instr.value) {
//           return void currentBlock.instructions.push(new JumpInstruction(copyState.returningBlockLabel))
//         }
//         currentBlock.instructions.push(new AllocInstruction(copyState.returningTarget, instr.type))
//         currentBlock.instructions.push(new StoreToAddressInstruction(copyState.returningTarget, instr.type, instr.value))
//         const jump = new JumpInstruction(copyState.returningBlockLabel)
//         currentBlock.instructions.push(jump)
//         return
//       } else if (instr instanceof LoadFromAddressInstruction) {
//         compilerAssert(instr.type !== VoidType, "Cannot load void type", { instr })
//       }
//       currentBlock.instructions.push(instr)
//     })
//   })

//   compilerAssert(yieldReturnBlockLabel, "No yield found")

//   const index = fn.blocks.findIndex(b => b.label === instrId.blockId)!
//   compilerAssert(index >= 0, "Block not found", { instrId, fn })
//   fn.blocks.splice(index + 1, 0, ...newBlocks)

// }

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


const mapInstructions = (codegen: CodeGenerator, mapping: RegisterMapping, instructions: IRInstruction[], func: (instr: IRInstruction) => void) => {
  const newRegister = (prev: string) => {
    const reg = codegen.newRegister()
    mapping[prev] = reg
    return reg
  }
  instructions.forEach((instr) => {
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
  })
}
