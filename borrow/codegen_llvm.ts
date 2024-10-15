import { externalBuiltinBindings } from "../src/compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BlockAst, BoolType, CallAst, Capability, CompiledFunction, ConcreteClassType, ConstructorAst, DefaultConsAst, DoubleType, FileWriter, FloatType, FunctionType, GlobalCompilerState, IntType, LetAst, ListTypeConstructor, LlvmFunctionWriter, LlvmWriter, NeverType, NumberAst, ParameterizedType, Pointer, PrimitiveType, RawPointerType, Register, SetAst, SourceLocation, StatementsAst, StringType, Type, TypeField, UserCallAst, ValueFieldAst, VoidType, compilerAssert, isAst, isType, textColors, u64Type, u8Type } from "../src/defs";
import { AccessInstruction, AllocInstruction, BinaryOperationInstruction, CallInstruction, CommentInstruction, ConditionalJumpInstruction, EndAccessInstruction, formatInstruction, FunctionBlock, GetFieldPointerInstruction, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, MarkInitializedInstruction, PhiInstruction, ReturnInstruction, StoreToAddressInstruction } from "./defs";

// Some useful commands
//
// Compile and run
//   NAME=generated; /opt/homebrew/opt/llvm/bin/llc $NAME.ll -O3 -o $NAME.s && clang $NAME.s -o $NAME.native && ./$NAME.native
// Optimise
//   NAME=generated; /opt/homebrew/opt/llvm/bin/opt -O1 -S $NAME.ll -o $NAME.opt.ll
//   NAME=generated; /opt/homebrew/opt/llvm/bin/opt -O3 -S $NAME.ll -o $NAME.opt.ll
//

// NAME=generated; /opt/homebrew/opt/llvm/bin/llc -filetype=asm -mtriple=wasm32-unknown-unknown $NAME.ll -o $NAME.wat

// TOOD:
// `function<eager-inv>(`,
// // `  lower-expect,`,
// `  sroa<>,`,
// `  early-cse,`,
// `  simplifycfg`,
// `),`,


const operatorMapSignedInt: {[key: string]:string} = {
  "+": "add",
  "-": "sub",
  "*": "mul",
  "/": "sdiv", // signed
  "==": "icmp eq",
  "!=": "icmp ne",

  "<<": "shl",
  ">>": "lshr", // Logical shift right

  "&": "and",
  "|": "or",

  // Signed
  ">": "icmp sgt",
  "<": "icmp slt",
  "<=": "icmp sle",
  ">=": "icmp sge",

  "mod": "srem"
}

const operatorMapUnsignedInt: {[key: string]:string} = {
  "+": "add",
  "-": "sub",
  "*": "mul",
  "/": "udiv", // unsigned
  "==": "icmp eq",
  "!=": "icmp ne",

  "<<": "shl",
  ">>": "lshr", // Logical shift right

  "&": "and",
  "|": "or",

  // Signed
  ">": "icmp sgt",
  "<": "icmp slt",
  "<=": "icmp sle",
  ">=": "icmp sge",

  "mod": "urem"
}

const operatorMapFloat: {[key: string]:string} = {
  "+": "fadd",
  "-": "fsub",
  "*": "fmul",
  "/": "fdiv",

  // https://llvm.org/docs/LangRef.html#fcmp-instruction
  // O means ordered
  "==": "fcmp oeq",
  "!=": "fcmp one",
  ">": "fcmp ogt",
  "<": "fcmp olt",
  "<=": "fcmp ole",
  ">=": "fcmp oge",
}

const operatorMapLogical: {[key: string]:string} = {
  "&": "and",
  "|": "or",
  "==": "icmp eq",
  "!=": "icmp ne",
};

const log = (...args: any[]) => {
  if ((globalThis as any).logger) (globalThis as any).logger.log(...args)
}

const getPointerName = (writer: LlvmFunctionWriter, type: Type) => {
  if (type === RawPointerType) return 'ptr'
  if (type.typeInfo.isReferenceType) return 'ptr'
  return `${getTypeName(writer.writer, type)}*`
}

type RegisterResult = { register: Register }
type PointerResult = { pointer: Pointer }
export type LlvmResultValue = RegisterResult | PointerResult | null

const createPointer = (name: string, type: Type) => new Binding(name, type) as Pointer
const createRegister = (name: string, type: Type) => new Binding(name, type) as Register

export type LlvmAstWriterTable = {
  [A in Ast as A['key']]: (writer: LlvmFunctionWriter, ast: A) => LlvmResultValue;
}


type Writable = { writer: LlvmWriter, currentOutput: string[] }
const format = (writer: Writable, format: string, ...args: (string | number | Type | Ast | Binding | RegisterName)[]) => {
  let i = 0
  const s = format.replace(/\$/g, (x) => {
    const v = args[i++]
    
    if (typeof v === 'string') return v
    if (typeof v === 'number') return String(v)
    if (isType(v)) return getTypeName(writer.writer, v)
    if (v instanceof Binding) { return generateName(writer.writer, v) }
    if (v instanceof RegisterName) { 
      const reg = writer.writer.registers.get(v.name)
      compilerAssert(reg, "Register not found", { v })
      return generateName(writer.writer, reg)
    }
    compilerAssert(false, "Not supported in format", { format, v, str: String(v) })
  })
  writer.currentOutput.push(s)
}

const defineRegister = (writer: LlvmFunctionWriter, name: string, type: Type) => {
  const reg = createRegister(name, type)
  writer.writer.registers.set(name, reg)
  return reg
}

class RegisterName {
  constructor(public name: string) {}
}
const register = (name: string) => new RegisterName(name)
const getRegisterName = (writer: LlvmFunctionWriter, name: string) => {
  const reg = writer.writer.registers.get(name)
  compilerAssert(reg, "Register not found", { name })
  return generateName(writer.writer, reg)
}

const instructionWriter = {
  loadconst: (writer: LlvmFunctionWriter, instr: LoadConstantInstruction) => {
    const dest = defineRegister(writer, instr.dest, instr.type)
    if (instr.type === RawPointerType) {
      compilerAssert(instr.value === 0, "Only null pointer allowed")
      format(writer, `  $ = bitcast ptr null to ptr; literal null\n`, dest)
      return
    }
    compilerAssert(instr.type === IntType || instr.type === u8Type || instr.type === u64Type || instr.type === FloatType || instr.type === DoubleType, "Expected number type got $type", { instr, type: instr.type })
    
    if (instr.type === FloatType) {
      format(writer, `  $ = fadd $ 0.0, $ ; literal $\n`, dest, instr.type, floatToLlvmHex(instr.value), instr.value)
    } else if (instr.type === DoubleType) {
      format(writer, `  $ = fadd $ 0.0, $ ; literal $\n`, dest, instr.type, doubleToLlvmHex(instr.value), instr.value)
    } else {
      format(writer, `  $ = add $ 0, $ ; literal $\n`, dest, instr.type, instr.value, instr.value)
    }
  },

  call: (writer: LlvmFunctionWriter, instr: CallInstruction) => {
    const funcName = generateName(writer.writer, instr.binding)
    const args = instr.args
    const fn = writer.writer.globalCompilerState.compiledFunctions.get(instr.binding)
    compilerAssert(fn, "No function found")
    const returnType = fn.returnType
    const result = instr.type !== VoidType && defineRegister(writer, "", returnType)
    const paramTypes = fn.parameters.map(x => x.type)
    const argStr = paramTypes.map((type, i) => {
      const reg = writer.writer.registers.get(args[i])
      compilerAssert(reg, "Register not found", { instr })

      return `${getTypeName(writer.writer, reg.type)} ${generateName(writer.writer, reg)}`
    }).join(", ")

    if (result) { format(writer, `  $ = `, result) }
    else { format(writer, `  `) }

    format(writer, "call $ $($)\n", returnType, funcName, argStr)
    if (result) format(writer, "  store $ $, $ $\n", returnType, result, 'ptr', register(instr.target!))
  },

  return: (writer: LlvmFunctionWriter, instr: ReturnInstruction) => {
    if (!instr.value) { format(writer, "  ret void\n"); return null }
    const reg = writer.writer.registers.get(instr.value)
    compilerAssert(reg, "Register not found", { instr })
    format(writer, "  ret $ $\n", reg.type, reg)
    return null
  },

  binaryop: (writer: LlvmFunctionWriter, instr: BinaryOperationInstruction) => {
    const dest = defineRegister(writer, instr.dest, instr.type)
    const operatorMap = instr.type === IntType || instr.type === u64Type ? operatorMapSignedInt : instr.type === FloatType || instr.type === DoubleType ? operatorMapFloat : operatorMapLogical
    const operator = operatorMap[instr.operator]
    compilerAssert(operator, "Operator not found", { instr })
    format(writer, "  $ = $ $ $, $\n", dest, operator, instr.type, register(instr.left), register(instr.right))
  },

  store_to_address: (writer: LlvmFunctionWriter, instr: StoreToAddressInstruction) => {
    format(writer, "  store $ $, $ $\n", instr.type, register(instr.source), 'ptr', register(instr.address))
  },

  load_from_address: (writer: LlvmFunctionWriter, instr: LoadFromAddressInstruction) => {
    const addr = writer.writer.registers.get(instr.address)
    compilerAssert(addr, "Register not found", { instr })
    defineRegister(writer, instr.dest, instr.type)
    format(writer, "  $ = load $, $ $\n", register(instr.dest), instr.type, getPointerName(writer, addr.type), addr)
  },

  getfieldptr: (writer: LlvmFunctionWriter, instr: GetFieldPointerInstruction) => {
    const source = writer.writer.registers.get(instr.address)
    compilerAssert(source, "Register not found", { instr })
    const field = instr.field
    const dest = defineRegister(writer, instr.dest, RawPointerType)
    const sourceType = getDataTypeName(writer.writer, field.sourceType);
    const pointerType = getPointerName(writer, field.sourceType);
    format(writer, "  $ = getelementptr inbounds $, $ $, i32 0, i32 $\n", dest, sourceType, pointerType, source, field.index)
  },

  phi: (writer: LlvmFunctionWriter, instr: PhiInstruction) => {
    const dest = defineRegister(writer, instr.dest, instr.type)
    const sources = instr.sources.map(source => {
      const reg = writer.writer.registers.get(source.value)
      compilerAssert(reg, "Register not found", { instr })
      return `[ ${generateName(writer.writer, reg)}, ${getRegisterName(writer, source.block)} ]`
    }).join(", ")
    format(writer, "  $ = phi $ $\n", dest, instr.type, sources)
  },

  jump: (writer: LlvmFunctionWriter, instr: JumpInstruction) => {
    format(writer, "  br label $\n", register(instr.target))
  },

  cjump: (writer: LlvmFunctionWriter, instr: ConditionalJumpInstruction) => {
    format(writer, "  br i1 $, label $, label $\n", register(instr.condition), register(instr.targetLabel), register(instr.elseLabel))
  },

  alloc: (writer: LlvmFunctionWriter, instr: AllocInstruction) => {
    const dest = defineRegister(writer, instr.dest, RawPointerType)
    format(writer, "  $ = alloca $ ; $\n", dest, instr.type, getDataTypeName(writer.writer, instr.type))
  },

  comment: (writer: LlvmFunctionWriter, instr: CommentInstruction) => {
    format(writer, "  ; $\n", instr.comment)
  },
  
  access: (writer: LlvmFunctionWriter, instr: AccessInstruction) => {
    const reg = writer.writer.registers.get(instr.source)
    compilerAssert(reg, "Register not found", { reg, instr })
    writer.writer.registers.set(instr.dest, reg)
  },

  end_access: (writer: LlvmFunctionWriter, instr: EndAccessInstruction) => {
    // pass
  },

  mark_initialized: (writer: LlvmFunctionWriter, instr: MarkInitializedInstruction) => {
    // pass
  },
}
const writeInstructions = (writer: LlvmFunctionWriter, fnIr: FunctionBlock) => {
  
  fnIr.blocks.forEach((block, i) => {
    format(writer, "\n")
    format(writer, "$:\n", block.label)

    block.instructions.forEach(instr => {
      
      const f = (instructionWriter as any)[instr.irType]
      if (f) f(writer, instr)
      else format(writer, "  ; $ not implemented\n", instr.irType)
    })
  })
}

export const writeLlvmBytecode = (globalCompilerState: GlobalCompilerState, outputWriter: FileWriter) => {
  const bytecodeWriter: LlvmWriter = {
    functions: [],
    globalCompilerState,
    functionToIndex: new Map(),
    typeSizes: new Map(),
    globals: new Map(),
    globalNames: new Map(),
    globalNameToBinding: new Map(),
    nextGlobalSlot: 0,
    outputHeaders: [],
    outputStrings: [],
    outputWriter,
    writer: null!,
    currentOutput: null!,
    mallocBinding: null!,
    astVisitMap: new Map(),
    registers: new Map(),
  }
  bytecodeWriter.writer = bytecodeWriter
  bytecodeWriter.currentOutput = bytecodeWriter.outputHeaders

  const insertGlobal = (binding: Binding | Type, name: string) => {
    compilerAssert(!bytecodeWriter.globalNameToBinding.has(name), `Already generated ${name}`)
    bytecodeWriter.globalNames.set(binding, name)
    bytecodeWriter.globalNameToBinding.set(name, binding)
  }
  insertGlobal(VoidType, "void")
  insertGlobal(IntType, "i32")
  insertGlobal(u64Type, "i64")
  insertGlobal(u8Type, "i8")
  insertGlobal(BoolType, "i1")
  insertGlobal(FloatType, "float")
  insertGlobal(DoubleType, "double")
  insertGlobal(RawPointerType, "ptr")
  bytecodeWriter.globalNames.set(NeverType, "void")
  insertGlobal(externalBuiltinBindings.printf, "@printf")

  let malloc = globalCompilerState.externalDefinitions.find(x => x.name === 'malloc')
  if (malloc) bytecodeWriter.mallocBinding = malloc.binding
  else {
    bytecodeWriter.mallocBinding = new Binding("malloc", FunctionType)
    globalCompilerState.externalDefinitions.push({ name: 'malloc', binding: bytecodeWriter.mallocBinding, paramTypes: [IntType], returnType: RawPointerType, paramHash: "" })
  }

  globalCompilerState.externalDefinitions.forEach(external => {
    // The global name is important to link
    insertGlobal(external.binding, `@${external.name}`)

    const args = external.paramTypes.map((type, i) => getTypeName(bytecodeWriter, type) ).join(", ")
    format(bytecodeWriter, "declare $ $($)\n", external.returnType, external.binding, args)
  })

  insertGlobal(globalCompilerState.initializerFunctionBinding, `@${globalCompilerState.initializerFunctionBinding.name}`)

  Object.entries(globalCompilerState.exports).forEach(([name, compiledFunction]) => {
    insertGlobal(compiledFunction.binding, `@${name}`)
  })

  bytecodeWriter.outputHeaders.push("declare i32 @printf(i8*, ...)\n\n")
  bytecodeWriter.outputHeaders.push(`\n`)

  globalCompilerState.globalLets.forEach(globalLet => {
    const name = generateName(bytecodeWriter, globalLet.binding, true)
    format(bytecodeWriter, "$ = global $ $\n", name, globalLet.binding.type, defaultValueLiteral(bytecodeWriter, globalLet.binding.type))
  })
  bytecodeWriter.outputHeaders.push(`\n`)

  Array.from(globalCompilerState.compiledFunctions.values()).map(func => {
    generateName(bytecodeWriter, func.binding, true)
    const fnIr = globalCompilerState.compiledIr.get(func.binding)
    compilerAssert(fnIr, `No instructions found for ${func.binding.name}`)
    const funcWriter = writeLlvmBytecodeFunction(bytecodeWriter, func, fnIr)
    return funcWriter
  })

  bytecodeWriter.outputHeaders.forEach(str => {
    outputWriter.write(str)
  })
  outputWriter.write("\n\n")

  bytecodeWriter.outputStrings.forEach(str => {
    outputWriter.write(str)
  })

  return bytecodeWriter
}



const defaultValueLiteral = (writer: LlvmWriter, type: Type): string => {
  if (type === FloatType) return floatToLlvmHex(0)
  else if (type === DoubleType) return doubleToLlvmHex(0)
  else if (type === IntType || type === u64Type) return '0'
  else if (type === RawPointerType) return 'null'
  else if (type.typeInfo.isReferenceType) return 'null'
  else {
    const fields = type.typeInfo.fields.map(x => 
      `${getTypeName(writer, x.fieldType)} ${defaultValueLiteral(writer, x.fieldType)}`)
    return `{ ${fields.join(", ")} }`
  }
}

const generateName = (writer: LlvmWriter, binding: Binding, global = false) => {
  if (writer.globalNames.get(binding)) {
    return writer.globalNames.get(binding)!
  }
  let name = binding.name.replace(/[^a-zA-Z0-9_\.]/g, ' ').trim().replace(/ +/g, '_')
  if (global) name = `@${name}`
  else name = `%${name}`

  let newName = name; let index = 0

  while (newName.length <= 1 || writer.globalNameToBinding.get(newName)) { newName = `${name}_${index++}` }
  writer.globalNames.set(binding, newName)
  writer.globalNameToBinding.set(newName, binding)
  return newName
}

const getTypeName = (writer: LlvmWriter, obj: Type): string => {
  if (obj.typeInfo.isReferenceType) return 'ptr'
  return getDataTypeName(writer, obj)
}

const getDataTypeName = (writer: LlvmWriter, obj: Type): string => {
  if (writer.globalNames.get(obj)) {
    return writer.globalNames.get(obj)!
  }
  const name = generateName(writer, new Binding(`%struct.${obj.shortName}`, VoidType))
  writer.globalNames.set(obj, name)

  const fields = obj.typeInfo.fields.map(x => getTypeName(writer, x.fieldType)) // May be recursive so do it before emitting

  writer.outputHeaders.push(`${name} = type { `)
  obj.typeInfo.fields.forEach((field, i) => {
    if (i !== 0) writer.outputHeaders.push(', ')
    writer.outputHeaders.push(fields[i])
  })
  if (obj.typeInfo.variantPadding) writer.outputHeaders.push(`, [${obj.typeInfo.variantPadding} x i8]`)
  writer.outputHeaders.push(` }\n`)

  return name
}
const writeLlvmBytecodeFunction = (bytecodeWriter: LlvmWriter, func: CompiledFunction, fnIr: FunctionBlock) => {
  log("\nWriting func", func.functionDefinition.debugName, "\n")
  const funcWriter: LlvmFunctionWriter = {
    writer: bytecodeWriter,
    function: func,
    currentBlockLabel: null!,
    blocks: [],
    constantsByType: new Map(),
    outputFunctionBody: [],
    outputFunctionHeaders: [],
    currentOutput: null!,
    printNextStatement: false,
  }

  const isMain = bytecodeWriter.globalCompilerState.entryFunction === func
  const name = isMain ? "@main" : generateName(bytecodeWriter, func.binding)
  
  funcWriter.currentOutput = bytecodeWriter.outputStrings

  const argValueBindings = func.parameters.map(param => {
    if (param.reference) return param.binding
    return new Binding(param.binding.name, param.binding.type)
  })

  format(funcWriter, `; `)
  func.parameters.forEach((param, i) => {
    if (i !== 0) format(funcWriter, ", ")
    format(funcWriter, `$: $ $`, param.binding.name, param.capability.toLowerCase(), param.type.shortName)
  })
  format(funcWriter, `\n`)
  format(funcWriter, `define $ $(`, func.returnType, name)

  func.parameters.forEach((param, i) => {
    if (i !== 0) format(funcWriter, ", ")
    // @ParameterPassing
    format(funcWriter, `$ $`, param.passingType, argValueBindings[i])
  })
  format(funcWriter, `) {\n`, func.returnType, name)

  func.parameters.forEach((param, i) => {
    const regName = fnIr.parameterRegisters[i]
    const reg = defineRegister(funcWriter, regName, param.passingType)
    generateName(bytecodeWriter, reg)

    // @ParameterPassing
    if (!param.reference) {
      format(funcWriter, "  $ = alloca $ ; $\n", reg, reg.type, getDataTypeName(funcWriter.writer, reg.type))
      format(funcWriter, "  store $ $, $ $\n", param.passingType, argValueBindings[i], 'ptr', reg)
    } else {
      format(funcWriter, "  $ = bitcast ptr $ to ptr ; $*\n", reg, argValueBindings[i], argValueBindings[i].type.shortName)
    }
  })
  format(funcWriter, "\n")
  format(funcWriter, "  br label %$\n", fnIr.blocks[0].label)

  fnIr.blocks.forEach(block => {
    defineRegister(funcWriter, block.label, VoidType)
  })

  writeInstructions(funcWriter, fnIr)
  
  bytecodeWriter.outputStrings.push(...funcWriter.outputFunctionBody)

  if (isMain) { // hardcode for now
    format(funcWriter, `  ret i32 0\n`)
  } else if (func.body.type === NeverType) {
    format(funcWriter, `  unreachable\n`)
  } else if (func.returnType !== VoidType) {
    // const v = toRegister(funcWriter, result)
    // format(funcWriter, `  ret $ $\n`, func.returnType, v)
  } else {
    format(funcWriter, `  ret void\n`)
  }
  format(funcWriter, `}\n\n`)

  return funcWriter
};


const numberToDoubleBitString = (f: number) => {
  const buffer = new ArrayBuffer(8)
  const floatView = new Float64Array(buffer)
  floatView[0] = f
  const intView = new DataView(buffer)
  const intBitsLow = intView.getUint32(0, true) // true for little-endian, lower part
  const intBitsHigh = intView.getUint32(4, true) // true for little-endian, higher part
  return (intBitsHigh.toString(2).padStart(32, '0') + intBitsLow.toString(2).padStart(32, '0'))
}

const bitStringToFloat = (bitString: string) => {
  // https://llvm.org/docs/LangRef.html#id1977
  // TODO: What was I thinking here? Do it properly
  
  // LLVM has some weird behaviour where a float constant is 
  // written in 64 bit but the exponent is rounded to 23 bits
  // otherwise it won't compile. Easiest way is string manipulations

  // So it looks like
  //  1 bit sign | 11 bit mantissa | 23 bit exponent | 29 bit zeros
  
  // Testcases
  // 0.001 => 0x3F50624DE0000000
  // 3.14159 => 0x400921FA00000000

  const cap = 1 + 11 + 23
  if (bitString[cap] === '1') {
    const rounded = parseInt(bitString.substring(0, cap), 2) + 1
    return rounded.toString(2).padStart(cap, '0').padEnd(64, '0')
  }
  return bitString.substring(0, cap).padEnd(64, '0')
}

const bitsToHex = (bits: string) => `0x${parseInt(bits, 2).toString(16).padStart(16, '0').toUpperCase()}`
const doubleToLlvmHex = (f: number) => bitsToHex(numberToDoubleBitString(f))
const floatToLlvmHex = (f: number) => bitsToHex(bitStringToFloat(numberToDoubleBitString(f)))
