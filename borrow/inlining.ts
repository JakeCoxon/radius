import { Capability, GlobalCompilerState, RawPointerType, VoidType, compilerAssert } from "../src/defs"; // prettier-ignore
import { CodeGenerator } from '../borrow/codegen_ir';
import { AccessInstruction, AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, CommentInstruction, ConditionalJumpInstruction, EndAccessInstruction, FunctionBlock, GetFieldPointerInstruction, InstructionId, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, MarkInitializedInstruction, Module, PhiInstruction, PhiSource, PointerOffsetInstruction, printIR, ProjectBundleInstruction, ReturnInstruction, StoreToAddressInstruction, YieldInstruction } from '../borrow/defs';

const findInstructionId = (fn: FunctionBlock, pred: (instr: IRInstruction) => boolean) => {
  for (const block of fn.blocks) {
    for (let i = 0; i < block.instructions.length; i++) {
      const instr = block.instructions[i]
      if (pred(instr)) {
        return new InstructionId(block.label, i)
      }
    }
  }
  return null
}
const filterInstructions = (fn: FunctionBlock, pred: (instr: IRInstruction) => boolean) => {
  const result: InstructionId[] = []
  for (const block of fn.blocks) {
    for (let i = 0; i < block.instructions.length; i++) {
      const instr = block.instructions[i]
      if (pred(instr)) {
        result.push(new InstructionId(block.label, i))
      }
    }
  }
  return result
}

const getInstructionById = (fn: FunctionBlock, instrId: InstructionId) => {
  const block = fn.blocks.find(b => b.label === instrId.blockId)!
  return block.instructions[instrId.instrId]
}

export const inlineProjectBundlesPass = (globalCompiler: GlobalCompilerState, codegen: CodeGenerator) => {
  globalCompiler.compiledFunctions.forEach((func) => {
    if (!func.body) return

    const fn = globalCompiler.compiledIr.get(func.binding)!
    if (func.functionDefinition.keywords?.includes("subscript")) return

    while (true) {
      const projectInstrId = findInstructionId(fn, instr => instr instanceof ProjectBundleInstruction)
      if (!projectInstrId) break
      const projectInstr = getInstructionById(fn, projectInstrId) as ProjectBundleInstruction
      const cap = projectInstr.capabilities[0]
      const binding = projectInstr.funcs[cap]
      compilerAssert(binding, "Capability not available", { projectInstr, name: func.functionDefinition.debugName })
      const subscriptIr = globalCompiler.compiledIr.get(binding)!
      const returningTarget = projectInstr.target
      inlineIr(globalCompiler, codegen, fn, projectInstrId, subscriptIr, returningTarget)
    }
  })
}

export const inlineIr = (globalCompiler: GlobalCompilerState, codegen: CodeGenerator, fn: FunctionBlock, instrId: InstructionId, newIr: FunctionBlock, returningTarget: string) => {

  console.log("\n\n//////////// Inlining")
  printIR(newIr.blocks)
  console.log("\n\n//////////// Previous")
  printIR(fn.blocks)
  // There are some assumptions about the CFG that we make during this function.
  // For example we assume that the replaced blocks cannot be entered from any other block.
  // This is because we don't update the predecessors of the replaced blocks.

  const projectInstr = fn.blocks.find(b => b.label === instrId.blockId)!.instructions[instrId.instrId] as ProjectBundleInstruction

  const mapping: RegisterMapping = {}

  const [prev, next] = splitBlockAtInstr(codegen, fn, instrId)
  prev.instructions.pop() // Remove the jump instruction, we'll replace it
  prev.instructions.push(new CommentInstruction(`Project bundle inlining parameters`))
  prev.instructions.push(...createParameterInstructions(newIr, projectInstr, codegen, mapping))
  next.instructions.shift() // Remove the project instruction
  
  const endInstrs = filterInstructions(fn, instr => instr instanceof EndAccessInstruction && instr.source === projectInstr.target)
  compilerAssert(endInstrs.length > 0, "End instruction not found", { instrId })
  compilerAssert(endInstrs.length === 1, "Multiple end instructions not supported yet", { instrId })
  
  const endInstrId = findInstructionId(fn, instr => instr instanceof EndAccessInstruction && instr.source === projectInstr.target)
  compilerAssert(endInstrId, "End instruction not found", { instrId, fn })

  const [endPrev, endNext] = splitBlockAtInstr(codegen, fn, endInstrId)
  endNext.instructions.shift() // Remove the end_access instruction

  const returningBlockLabel = endNext.label
  const yieldBlockLabel = endPrev.label
  const state: CopyBlockState = { mapping, returningBlockLabel, returningTarget, yieldBlockLabel, yieldTarget: projectInstr.target }
  insertCopiedBlocks(codegen, mapping, fn, instrId, newIr, state)

  const newEntry = mapping[newIr.blocks[0].label]
  compilerAssert(newEntry, "Entry not found", { newIr, mapping })
  prev.instructions.push(new JumpInstruction(newEntry))

  console.log("\n\n//////////// Split")
  printIR(fn.blocks)

}
const createParameterInstructions = (newIr: FunctionBlock, projectInstr: ProjectBundleInstruction, codegen: CodeGenerator, mapping: RegisterMapping) => {
  const insertedInstructions: IRInstruction[] = [];
  newIr.parameterRegisters.forEach((reg, i) => {
    mapping[reg] = codegen.newRegister();
    const passingType = newIr.params[i].passingType;
    const source = i === 0 ? projectInstr.source : projectInstr.operands[i - 1];

    // @ParameterPassing
    if (passingType === RawPointerType) {
      insertedInstructions.push(new AssignInstruction(mapping[reg], passingType, source));
      return;
    }
    insertedInstructions.push(new AllocInstruction(mapping[reg], passingType));
    insertedInstructions.push(new StoreToAddressInstruction(mapping[reg], passingType, source));
  });
  return insertedInstructions;
}

type RegisterMapping = {
  [key: string]: string;
};
type CopyBlockState = {
  mapping: RegisterMapping;
  returningBlockLabel: string;
  returningTarget: string;
  yieldBlockLabel: string;
  yieldTarget: string;
};
const insertCopiedBlocks = (codegen: CodeGenerator, mapping: RegisterMapping, fn: FunctionBlock, instrId: InstructionId, newIr: FunctionBlock, copyState: CopyBlockState) => {

  newIr.blocks.forEach(b => {
    mapping[b.label] = codegen.newLabel()
  })

  const newBlocks: BasicBlock[] = []
  let currentBlock: BasicBlock
  let yieldReturnBlockLabel = ""
  newIr.blocks.forEach(b => {
    const label = mapping[b.label]
    currentBlock = new BasicBlock(label, [])
    newBlocks.push(currentBlock)
    mapInstructions(codegen, mapping, b.instructions, (instr) => {
      if (instr instanceof YieldInstruction) {
        compilerAssert(!yieldReturnBlockLabel, "Multiple yields not supported", { instr, copyState })
        currentBlock.instructions.push(new CommentInstruction(`Inlining yield ${instr.value} -> ${copyState.yieldTarget}`))
        if (instr.value) {
          currentBlock.instructions.push(new AssignInstruction(copyState.yieldTarget, instr.type, instr.value))
          // TODO: Do we need to store the value sometimes? or is it always assign?
          // currentBlock.instructions.push(new AllocInstruction(copyState.yieldTarget, instr.type))
          // currentBlock.instructions.push(new StoreToAddressInstruction(copyState.yieldTarget, instr.type, instr.value))
        }
        currentBlock.instructions.push(new JumpInstruction(copyState.yieldBlockLabel))
        currentBlock = new BasicBlock(codegen.newLabel(), [])
        yieldReturnBlockLabel = currentBlock.label

        const yieldFrom = fn.blocks.find(b => b.label === copyState.yieldBlockLabel)!
        yieldFrom.instructions.pop()
        yieldFrom.instructions.push(new JumpInstruction(yieldReturnBlockLabel))

        newBlocks.push(currentBlock)
        return
      } else if (instr instanceof ReturnInstruction) {
        // TODO: Support returning better by rewriting it as a block and break in the AST stage
        if (!instr.value) {
          return void currentBlock.instructions.push(new JumpInstruction(copyState.returningBlockLabel))
        }
        currentBlock.instructions.push(new AllocInstruction(copyState.returningTarget, instr.type))
        currentBlock.instructions.push(new StoreToAddressInstruction(copyState.returningTarget, instr.type, instr.value))
        const jump = new JumpInstruction(copyState.returningBlockLabel)
        currentBlock.instructions.push(jump)
        return
      } else if (instr instanceof LoadFromAddressInstruction) {
        compilerAssert(instr.type !== VoidType, "Cannot load void type", { instr })
      }
      currentBlock.instructions.push(instr)
    })
  })

  compilerAssert(yieldReturnBlockLabel, "No yield found")

  const index = fn.blocks.findIndex(b => b.label === instrId.blockId)!
  compilerAssert(index >= 0, "Block not found", { instrId, fn })
  fn.blocks.splice(index + 1, 0, ...newBlocks)

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

const splitBlockAtInstr = (codegen: CodeGenerator, fn: FunctionBlock, instrId: InstructionId) => {
  const block = fn.blocks.find(b => b.label === instrId.blockId)!
  const instrIndex = instrId.instrId
  compilerAssert(instrIndex >= 0, "Instruction not found", { instrId, block })
  const newBlock = new BasicBlock(codegen.newLabel(), [])
  newBlock.instructions = block.instructions.slice(instrIndex)
  block.instructions = block.instructions.slice(0, instrIndex)
  const blockIndex = fn.blocks.indexOf(block) + 1
  fn.blocks.splice(blockIndex, 0, newBlock)
  fn.blocks[blockIndex - 1].instructions.push(new JumpInstruction(newBlock.label))
  return [block, newBlock]
}
