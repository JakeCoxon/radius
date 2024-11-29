import { Capability, GlobalCompilerState, RawPointerType, VoidType, compilerAssert } from "../src/defs"; // prettier-ignore
import { CodeGenerator } from '../borrow/codegen_ir';
import { AccessInstruction, AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, CommentInstruction, ConditionalJumpInstruction, EndAccessInstruction, FunctionBlock, GetFieldPointerInstruction, InstructionId, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, MarkInitializedInstruction, Module, PhiInstruction, PhiSource, PointerOffsetInstruction, printIR, ProjectBundleInstruction, ReturnInstruction, StoreToAddressInstruction, YieldInstruction } from '../borrow/defs';


export const inlineProjectBundlesPass = (globalCompiler: GlobalCompilerState, codegen: CodeGenerator) => {
  globalCompiler.compiledFunctions.forEach((func) => {
    if (!func.body) return

    const fn = globalCompiler.compiledIr.get(func.binding)!
    if (func.functionDefinition.keywords?.includes("subscript")) return

    retry: while (true) {
      for (const block of fn.blocks) {
        for (let i = 0; i < block.instructions.length; i++) {
          const instr = block.instructions[i]
          if (instr instanceof ProjectBundleInstruction) {
            const cap = instr.capabilities[0]
            const binding = instr.funcs[cap]
            compilerAssert(binding, "Capability not available", { instr, name: func.functionDefinition.debugName })
            const subscriptIr = globalCompiler.compiledIr.get(binding)!
            const instrId = new InstructionId(block.label, i)
            const returningTarget = instr.target
            inlineIr(globalCompiler, codegen, fn, instrId, subscriptIr, returningTarget)
            continue retry
          }
        }
      }
      break
    }
  })
}

export const inlineIr = (globalCompiler: GlobalCompilerState, codegen: CodeGenerator, fn: FunctionBlock, instrId: InstructionId, newIr: FunctionBlock, returningTarget: string) => {

  console.log("\n\n//////////// Inlining")
  printIR(newIr.blocks)
  console.log("\n\n//////////// Previous")
  printIR(fn.blocks)

  const projectInstr = fn.blocks.find(b => b.label === instrId.blockId)!.instructions[instrId.instrId] as ProjectBundleInstruction


  const [prev, next1] = splitBlockAtInstr(codegen, fn, instrId)
  
  const endInstrId = (() => {
    for (let i = 0; i < fn.blocks.length; i++) {
      const block = fn.blocks[i]
      for (let j = 0; j < block.instructions.length; j++) {
        const instr = block.instructions[j]
        if (instr instanceof EndAccessInstruction) {
          if (instr.source === projectInstr.target) {
            return new InstructionId(block.label, j)
          }
        }
      }
    }
    return null
  })()

  compilerAssert(endInstrId, "End instruction not found", { instrId, fn })
  compilerAssert(endInstrId.blockId === next1.label, "End instruction not in the same block", { instrId, endInstrId, fn })
  const [middle, next] = splitBlockAtInstr(codegen, fn, endInstrId)

  prev.instructions.pop() // Remove the jump instruction, we'll add another later
  middle.instructions = middle.instructions.slice(1) // Remove the project instruction
  next.instructions = next.instructions.slice(1) // Remove the end instruction

  const returningBlockLabel = next.label
  const mapping: RegisterMapping = {}
  newIr.parameterRegisters.forEach((reg, i) => {
    mapping[reg] = codegen.newRegister()
    const passingType = newIr.params[i].passingType
    const source = i === 0 ? projectInstr.source : projectInstr.operands[i - 1]

    // @ParameterPassing
    if (passingType === RawPointerType) {
      prev.instructions.push(new AssignInstruction(mapping[reg], passingType, source))
      return
    }
    prev.instructions.push(new AllocInstruction(mapping[reg], passingType))
    prev.instructions.push(new StoreToAddressInstruction(mapping[reg], passingType, source))
  })
  const state: CopyBlockState = { mapping, returningBlockLabel, returningTarget, yieldBlockLabel: middle.label, yieldTarget: projectInstr.target }
  insertCopiedBlocks(codegen, mapping, fn, instrId, newIr, state)


  const newEntry = mapping[newIr.blocks[0].label]
  prev.instructions.push(new JumpInstruction(newEntry))


  console.log("\n\n//////////// Split")
  printIR(fn.blocks)

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
