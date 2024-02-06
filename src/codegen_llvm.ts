import { externals } from "./compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BlockAst, BoolType, CallAst, CompiledFunction, ConcreteClassType, ConstructorAst, DefaultConsAst, DoubleType, FileWriter, FloatType, FunctionType, GlobalCompilerState, IntType, ListTypeConstructor, LlvmFunctionWriter, LlvmWriter, NumberAst, ParameterizedType, Pointer, PrimitiveType, RawPointerType, Register, SourceLocation, StatementsAst, StringType, Type, TypeField, UserCallAst, ValueFieldAst, VoidType, compilerAssert, isAst, isType, textColors } from "./defs";

// Some useful commands
//
// Compile and run
//   NAME=generated; /opt/homebrew/opt/llvm/bin/llc $NAME.ll -O3 -o $NAME.s && clang $NAME.s -o $NAME.native && ./$NAME.native
// Optimise
//   NAME=generated; /opt/homebrew/opt/llvm/bin/opt -O1 -S $NAME.ll -o $NAME.opt.ll
//   NAME=generated; /opt/homebrew/opt/llvm/bin/opt -O3 -S $NAME.ll -o $NAME.opt.ll
//

const operatorMap: {[key: string]:string} = {
  "+": "add",
  "-": "sub",
  "*": "mul",
  "/": "sdiv", // signed
  "==": "icmp eq",
  "!=": "icmp ne",

  // Signed
  ">": "icmp sgt",
  "<": "icmp slt",
  "<=": "icmp sle",
  ">=": "icmp sge",

  "&": "and",
  "|": "or",
  "<<": "shl",
  ">>": "lshr", // Logical shift right
};

const log = (...args: any[]) => {
  if ((globalThis as any).logger) (globalThis as any).logger.log(...args)
}

const writeExpr = (writer: LlvmFunctionWriter, ast: Ast) => {
  compilerAssert(astWriter[ast.key], `Not implemented ast writer '${ast.key}'`)
  const toPrint = !(ast instanceof StatementsAst || ast instanceof BlockAst) && writer.printNextStatement
  if (toPrint) {
    writer.printNextStatement = false
    const line = ast.location.source?.input.split("\n").find((x, i) => i + 1 === ast.location.line)
    if (line) {
      format(writer, "; $ | ($) $\n", ast.location.line, ast.key, line.trim())
    } else format(writer, `; $\n`, ast.key)
  }
  const result = astWriter[ast.key](writer, ast as any)
  if (toPrint) format(writer, "\n")
  return result
};

const toStatements = (ast: Ast) => {
  if (ast instanceof StatementsAst) return ast
  return new StatementsAst(ast.type, ast.location, [ast])
}

const toRegister = (writer: LlvmFunctionWriter, v: LlvmResultValue): Register => {
  compilerAssert(v, "Result was a void")
  if ('register' in v) return v.register
  const name = createRegister("", VoidType)
  // if (v.pointer.type === VoidType) compilerAssert(false, "")
  format(writer, "  $ = load $, ptr $\n", name, v.pointer.type, v.pointer)
  return name
}
const getPointerName = (writer: LlvmFunctionWriter, type: Type) => {
  if (type === RawPointerType) return 'ptr'
  return `${getTypeName(writer.writer, type)}*`
}

type RegisterResult = { register: Register }
type PointerResult = { pointer: Pointer }
export type LlvmResultValue = RegisterResult | PointerResult | null

const createPointer = (name: string, type: Type) => new Binding(name, type) as Pointer
const createRegister = (name: string, type: Type) => new Binding(name, type) as Register

const getBindingStorageType = (binding: Binding) => getStorageType(binding.type)
const getStorageType = (type: Type) => {
  return type.typeInfo.isReferenceType ? RawPointerType : type
}

export type LlvmAstWriterTable = {
  [A in Ast as A['key']]: (writer: LlvmFunctionWriter, ast: A) => LlvmResultValue;
}

const beginBasicBlock = (writer: LlvmFunctionWriter, label: Binding) => {
  format(writer, "$:\n", generateName(writer.writer, label).substring(1))
  writer.currentBlockLabel = label
}

const allocaHelper = (writer: LlvmFunctionWriter, pointer: Pointer, overrideType: Type | null = null) => {
  writer.currentOutput = writer.outputFunctionHeaders
  format(writer, "  $ = alloca $ ; $\n", pointer, overrideType ?? pointer.type, getDataTypeName(writer.writer, pointer.type))
  writer.currentOutput = writer.outputFunctionBody
  return pointer
}
const getElementPointer = (writer: LlvmFunctionWriter, pointer: Pointer, fieldPath: TypeField[]) => {
  const outType = fieldPath[fieldPath.length - 1].fieldType
  const loadFieldPtr = createPointer("", outType)
  const indicesStr = fieldPath.reduce((acc, field, i) => {
    return `${acc}${i == 0 ? '' : ', '}i32 ${field.index}`
  }, "")

  const sourceDataType = getDataTypeName(writer.writer, fieldPath[0].sourceType)
  format(writer, "  $ = getelementptr $, $ $, i32 0, $\n", loadFieldPtr, sourceDataType, 'ptr', pointer, indicesStr)
  return loadFieldPtr
}
const loadFieldHelper = (writer: LlvmFunctionWriter, leftResult: LlvmResultValue, fieldPath: TypeField[]) => {
  const loadField = (base: LlvmResultValue, field: TypeField): RegisterResult => {
    compilerAssert(base, "")
    const loadFieldPtr = createPointer("", VoidType)
    const loadFieldVal = createRegister("", field.fieldType)

    if ('pointer' in base) {
      format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, field.sourceType, getPointerName(writer, field.sourceType), base.pointer, field.index)
      format(writer, "  $ = load $, $ $\n", loadFieldVal, loadFieldVal.type, getPointerName(writer, loadFieldVal.type), loadFieldPtr)
    } else {
      format(writer, "  $ = extractvalue $ $, $\n", loadFieldVal, getDataTypeName(writer.writer, field.sourceType), base.register, String(field.index))
    }
    return { register: loadFieldVal }
  }
  const result = fieldPath.reduce((reg, field) => {
    return loadField(leftResult, field)
  }, leftResult)
  compilerAssert(result && 'register' in result)
  return result.register
}

const astWriter: LlvmAstWriterTable = {
  statements: (writer, ast) => {
    // TODO: Filter voids?
    let result: LlvmResultValue = undefined!
    ast.statements.forEach((expr, i) => {
      writer.printNextStatement = true
      result = writeExpr(writer, expr)
    })
    return result
  },
  string: (writer, ast) => {
    const constantName = generateName(writer.writer, new Binding("constant", VoidType), true)
    const escaped = ast.value.replace(/["\\]|[\x00-\x1F\x80-\xFF]/g, (str) => {
      return `\\${str.charCodeAt(0).toString(16).padStart(2, '0')}`
    })
    const length = ast.value.length + 1 // add null terminator
    writer.writer.outputHeaders.push(`${constantName} = private unnamed_addr constant [${length} x i8] c"${escaped}\\00"\n`)
    const pointer = allocaHelper(writer, createPointer("", ast.type))
    format(writer, `  store $ { i32 $, ptr $ }, ptr $\n`, ast.type, String(length - 1), constantName, pointer)
    return { pointer }
  },
  binding: (writer, ast) => {
    compilerAssert(ast.binding.type !== VoidType)
    const isArg = !!writer.function.argBindings.find(x => x === ast.binding)
    if (isArg) return { register: ast.binding as Register }
    return { pointer: ast.binding as Pointer }
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented", { ast })
    const storageType = getBindingStorageType(ast.binding)
    const pointer = allocaHelper(writer, ast.binding as Pointer, storageType)
    const result = toRegister(writer, writeExpr(writer, ast.value))
    format(writer, "  store $ $, ptr $\n", storageType, result, pointer)
    return null
  },
  set: (writer, ast) => {
    const result = toRegister(writer, writeExpr(writer, ast.value))
    const ptrName = ast.binding
    format(writer, "  store $ $, ptr $\n", ast.binding.type, result, ptrName)
    return null
  },
  number: (writer, ast) => {
    const valueName = createRegister("", ast.type)
    if (ast.type === RawPointerType) {
      compilerAssert(ast.value === 0, "Only null pointer allowed")
      format(writer, `  $ = bitcast ptr null to ptr; literal null\n`, valueName)
      return { register: valueName }
    }
    compilerAssert(ast.type === IntType || ast.type === FloatType || ast.type === DoubleType, "Expected number type got $type", { ast, type: ast.type })
    format(writer, `  $ = add $ 0, $ ; literal\n`, valueName, ast.type, ast.value)
    return { register: valueName }
  },
  bool: (writer, ast) => {
    const name = createRegister("", VoidType)
    format(writer, `  $ = add $ 0, $ ; literal\n`, name, ast.type, ast.value ? "1" : "0")
    return { register: name }
  },
  if: (writer, ast) => {
    const outName = ast.type !== VoidType ? createRegister("", VoidType) : undefined
    const thenLabel = new Binding(`if_then`, VoidType)
    const endLabel = new Binding(`if_end`, VoidType)
    const elseLabel = ast.falseBody ? new Binding(`if_else`, VoidType) : endLabel
    let thenVal: Register | undefined = undefined, elseVal: Register | undefined = undefined
    let thenFinalLabel: Binding | undefined = undefined, elseFinalLabel: Binding | undefined = undefined

    format(writer, `  br i1 $, label $, label $\n\n`, ast.expr, thenLabel, elseLabel)
    beginBasicBlock(writer, thenLabel)
    const trueResult = writeExpr(writer, toStatements(ast.trueBody))
    thenFinalLabel = writer.currentBlockLabel // Nested control flow have have changed current block
    if (outName) thenVal = toRegister(writer, trueResult)
    format(writer, `  br label $\n\n`, endLabel)

    if (ast.falseBody) {
      beginBasicBlock(writer, elseLabel)
      const falseResult = writeExpr(writer, toStatements(ast.falseBody))
      elseFinalLabel = writer.currentBlockLabel // Nested control flow have have changed current block
      if (outName) elseVal = toRegister(writer, falseResult)
      format(writer, `  br label $\n\n`, endLabel)
    }

    beginBasicBlock(writer, endLabel)

    if (!outName) return null
    compilerAssert(thenVal && elseVal && elseFinalLabel, "Expected 'then' and 'else' branch")
    format(writer, `  $ = phi $ [ $, $ ], [ $, $ ]\n`, outName, ast.type, thenVal, thenFinalLabel, elseVal, elseFinalLabel)
    return { register: outName }
  },
  and: (writer, ast) => {
    const outName = createRegister("", VoidType)
    const [a, b] = ast.args
    const secondOperand = new Binding(`and_second_operand`, VoidType)
    const resultFalse = new Binding(`and_result_false`, VoidType)
    const resultLabel = new Binding(`and_result`, VoidType)

    format(writer, `  br i1 $, label $, label $\n`, a, secondOperand, resultFalse)
    beginBasicBlock(writer, secondOperand)
    const bVal = toRegister(writer, writeExpr(writer, b))
    const secondOperandFinalName = writer.currentBlockLabel // Nested control flow have have changed current block
    format(writer, `  br label $\n`, resultLabel)
    beginBasicBlock(writer, resultFalse)
    format(writer, `  br label $\n`, resultLabel)
    beginBasicBlock(writer, resultLabel)
    format(writer, `  $ = phi $ [ false, $ ], [ $, $ ]\n`, outName, ast.type, resultFalse, bVal, secondOperandFinalName)
    return { register: outName }
  },
  or: (writer, ast) => {
    const outName = createRegister("", VoidType)
    const [a, b] = ast.args
    const secondOperand = new Binding(`or_second_operand`, VoidType)
    const resultTrue = new Binding(`or_result_true`, VoidType)
    const resultLabel = new Binding(`or_result`, VoidType)

    format(writer, `  br i1 $, label $, label $\n`, a, resultTrue, secondOperand)
    beginBasicBlock(writer, secondOperand)
    const bVal = toRegister(writer, writeExpr(writer, b))
    const secondOperandFinalName = writer.currentBlockLabel // Nested control flow have have changed current block
    format(writer, `  br label $\n`, resultLabel)
    beginBasicBlock(writer, resultTrue)
    format(writer, `  br label $\n`, resultLabel)
    beginBasicBlock(writer, resultLabel)
    format(writer, `  $ = phi $ [ true, $ ], [ $, $ ]\n`, outName, ast.type, resultTrue, bVal, secondOperandFinalName)
    writer.currentBlockLabel = resultLabel
    return { register: outName }
  },
  while: (writer, ast) => {
    const loopCondition = new Binding(`while_condition`, VoidType)
    const loopBody = new Binding(`while_body`, VoidType)
    const loopEnd = new Binding(`while_end`, VoidType)

    format(writer, `  br label $\n`, loopCondition)
    beginBasicBlock(writer, loopCondition)
    const condResult = writeExpr(writer, ast.condition)
    const aVal = toRegister(writer, condResult)
    format(writer, `  br i1 $, label $, label $\n\n`, aVal, loopBody, loopEnd)

    beginBasicBlock(writer, loopBody)
    writeExpr(writer, ast.body)
    format(writer, `  br label $\n\n`, loopCondition)

    beginBasicBlock(writer, loopEnd)
    return null
  },

  block: (writer, ast) => {
    writer.blocks.push({ binding: ast.binding }) // not strictly necessary?
    const result = writeExpr(writer, ast.body)
    writer.blocks.pop()!
    const blockEndLabel = ast.binding
    format(writer, `  br label $\n\n`, blockEndLabel)
    beginBasicBlock(writer, blockEndLabel)
    return result
  },
  break: (writer, ast) => {
    const block = writer.blocks.findLast(x => x.binding === ast.binding)
    compilerAssert(block, "Programmer error. Expected block") // Programmer error
    format(writer, `  br label $\n`, ast.binding)
    return null
  },
  call: (writer, ast) => {
    if (ast.func === externals.printf) {
      const name = createRegister("", VoidType)
      const argsString = ast.args.map((arg, i) => {
        const reg = toRegister(writer, writeExpr(writer, arg))
        return `${getTypeName(writer.writer, arg.type)} ${generateName(writer.writer, reg)}`
      }).join(", ")
      
      format(writer, "  $ = call i32 (i8*, ...) @printf($)\n", name, argsString)
      return { register: name }
    }

    if (ast.func === externals.sizeof) {
      compilerAssert(isType(ast.typeArgs[0]), "Expected type")
      const dataType = getDataTypeName(writer.writer, ast.typeArgs[0])
      const ptr = createRegister("", VoidType)
      const register = createRegister("sizeof", VoidType)
      format(writer, "  $ = getelementptr $*, ptr null, i32 1\n", ptr, dataType)
      format(writer, "  $ = ptrtoint $* $ to i32\n", register, dataType, ptr)
      return { register }
    }

    compilerAssert(false, "External call not implemented", { ast })
  },

  usercall: (writer, ast) => {
    const name = ast.type !== VoidType && createRegister("", VoidType)

    const argValues = ast.args.map(arg => {
      return toRegister(writer, writeExpr(writer, arg))
    })
    
    if (name) { format(writer, `  $ = `, name) }
    else { format(writer, `  `) }

    const args = ast.args.map((arg, i) => {
      return `${getTypeName(writer.writer, arg.type)} ${generateName(writer.writer, argValues[i])}`
    }).join(", ")
    format(writer, `call $ $($)\n`, ast.type, ast.binding, args)
    if (name) return { register: name }
    return null
  },
  operator: (writer, ast) => {
    const [a, b] = ast.args
    const name = createRegister("", VoidType)
    compilerAssert(a.type === b.type, "Expected types to be equal", { a, b })
    const op = operatorMap[ast.operator]
    compilerAssert(op, "Expected op", { ast })
    format(writer, `  $ = $ $ $, $\n`, name, op, a.type, a, b)
    return { register: name }
  },
  not: (writer, ast) => {
    const expr = ast.expr
    compilerAssert(expr.type === BoolType, "Expected bool")
    const name = createRegister("", VoidType)
    format(writer, `  $ = xor $ $, 1\n`, name, expr.type, expr)
    return { register: name }
  },
  constructor: (writer, ast) => {
    if (ast.type.typeInfo.isReferenceType) {
      // TODO: Break out ASTs?
      const structPtrPtr = allocaHelper(writer, createPointer("", RawPointerType))
      const dataType = getDataTypeName(writer.writer, ast.type)

      const size = new CallAst(IntType, SourceLocation.anon, externals.sizeof, [], [ast.type])
      const mallocCall = new UserCallAst(RawPointerType, SourceLocation.anon, writer.writer.mallocBinding, [size])
      const structMallocPtr = toRegister(writer, writeExpr(writer, mallocCall))

      format(writer, "  store ptr $, ptr $\n", structMallocPtr, structPtrPtr)
      ast.args.forEach((arg, index) => {
        const reg = toRegister(writer, writeExpr(writer, arg))

        const fieldPtr = createPointer('', VoidType)
        const field = ast.type.typeInfo.fields[index]

        format(writer, "  $ = getelementptr $, ptr $, i32 0, i32 $\n", fieldPtr, dataType, structMallocPtr, index)
        format(writer, "  store $ $, $ $\n", field.fieldType, reg, getPointerName(writer, field.fieldType), fieldPtr)
      })
      return { pointer: structPtrPtr }
    }

    const structPtr = allocaHelper(writer, createPointer("", ast.type))
    ast.args.forEach((arg, index) => {
      const reg = toRegister(writer, writeExpr(writer, arg))

      const fieldPtr = createPointer('', VoidType)
      const field = ast.type.typeInfo.fields[index]

      format(writer, "  $ = getelementptr $, ptr $, i32 0, i32 $\n", fieldPtr, ast.type, structPtr, index)
      format(writer, "  store $ $, $ $\n", field.fieldType, reg, getPointerName(writer, field.fieldType), fieldPtr)
    })
    return { pointer: structPtr }

  },
  valuefield: (writer, ast) => {
    const leftResult = writeExpr(writer, ast.left)
    const reg = loadFieldHelper(writer, leftResult, ast.fieldPath)
    return { register: reg }
  },
  field: (writer, ast) => {
    const leftResult = toRegister(writer, writeExpr(writer, ast.left))
    const register = createRegister("", ast.left.type)
    const dataType = getDataTypeName(writer.writer, register.type)
    format(writer, "  $ = load $, ptr $\n", register, dataType, leftResult)
    const reg = loadFieldHelper(writer, { register }, [ast.field])
    return { register: reg }
  },
  setvaluefield: (writer, ast) => {
    const res = writeExpr(writer, ast.left)
    compilerAssert(res && 'pointer' in res, "Expected pointer") // ast.left is binding so should always be a pointer
    const fieldPtr = getElementPointer(writer, res.pointer, ast.fieldPath)
    const finalField = ast.fieldPath[ast.fieldPath.length - 1]
    const valueReg = toRegister(writer, writeExpr(writer, ast.value))
    format(writer, "  store $ $, $ $\n", finalField.fieldType, valueReg, 'ptr', fieldPtr)
    return null
  },
  setfield: (writer, ast) => {
    const reg = toRegister(writer, writeExpr(writer, ast.left))
    const leftPtr = reg as unknown as Pointer // reinterpret reg as a pointer
    const fieldPtr = getElementPointer(writer, leftPtr, [ast.field])
    const valueReg = toRegister(writer, writeExpr(writer, ast.value))
    format(writer, "  store $ $, $ $\n", ast.field.fieldType, valueReg, 'ptr', fieldPtr)
    return null
  },
  cast: (writer, ast) => {
    const op = (() => {
      if (ast.expr.type === IntType && ast.type === FloatType) return 'sitofp i32 $ to float' 
      if (ast.expr.type === IntType && ast.type === DoubleType) return 'sitofp i32 $ to double'
      if (ast.expr.type === FloatType && ast.type === IntType) return 'fptosi float $ to i32'
      if (ast.expr.type === DoubleType && ast.type === IntType) return 'fptosi double $ to i32'
      compilerAssert(false, "Invalid cast conversion")
    })()
    const register = createRegister("", ast.type)
    const input = toRegister(writer, writeExpr(writer, ast.expr))
    format(writer, `  $ = ${op}\n`, register, input)
    return { register }
  },
  defaultcons: (writer, ast) => {
    if (ast.type === IntType || ast.type === FloatType || ast.type === DoubleType || ast.type === RawPointerType) {
      return writeExpr(writer, new NumberAst(ast.type, ast.location, 0))
    }
    const fields = ast.type.typeInfo.fields.map(x => new DefaultConsAst(x.fieldType, ast.location))
    return writeExpr(writer, new ConstructorAst(ast.type, ast.location, fields))
  },
  list: (writer, ast) => {
    compilerAssert(false, "Not implemented 'list'", { ast })
  },
  subscript: (writer, ast) => {
    const reg = toRegister(writer, writeExpr(writer, ast.left))
    const index = toRegister(writer, writeExpr(writer, ast.right))
    const pointer = createPointer("", ast.type)
    format(writer, "  $ = getelementptr $, $ $, i32 $\n", pointer, ast.type, 'ptr', reg, index)
    return { pointer }
  },
  setsubscript: (writer, ast) => {
    const reg = toRegister(writer, writeExpr(writer, ast.left))
    const index = toRegister(writer, writeExpr(writer, ast.right))
    const value = toRegister(writer, writeExpr(writer, ast.value))
    const pointer = createPointer("", ast.value.type)
    format(writer, "  $ = getelementptr $, $ $, i32 $\n", pointer, ast.value.type, 'ptr', reg, index)
    format(writer, "  store $ $, $ $\n", ast.value.type, value, 'ptr', pointer)
    return null
  },
  deref: (writer, ast) => {
    // TODO: These are the same as value and setvalue. we can merge them
    const reg = toRegister(writer, writeExpr(writer, ast.left))
    const leftPtr = reg as unknown as Pointer // reinterpret reg as a pointer
    const fieldPtr = getElementPointer(writer, leftPtr, ast.fieldPath)
    return { pointer: fieldPtr }
  },
  setderef: (writer, ast) => {
    const reg = toRegister(writer, writeExpr(writer, ast.left))
    const leftPtr = reg as unknown as Pointer // reinterpret reg as a pointer
    const fieldPtr = getElementPointer(writer, leftPtr, ast.fieldPath)
    const value = toRegister(writer, writeExpr(writer, ast.value))
    format(writer, "  store $ $, $ $\n", ast.value.type, value, 'ptr', fieldPtr)
    return null
  },
  return: (writer, ast) => {
    if (!ast.expr) { format(writer, "  ret void\n"); return null }
    const reg = toRegister(writer, writeExpr(writer, ast.expr))
    format(writer, "  ret $ $\n", reg.type, reg)
    return null
  },
  address: (writer, ast) => {
    return { register: ast.binding as Register }
  },
  void: (writer, ast) => {
    return null
  }
};

type Writable = { writer: LlvmWriter, currentOutput: string[] }
const format = (writer: Writable, format: string, ...args: (string | number | Type | Ast | Binding)[]) => {
  let i = 0
  const s = format.replace(/\$/g, (x) => {
    const v = args[i++]
    
    if (typeof v === 'string') return v
    if (typeof v === 'number') return String(v)
    if (isType(v)) return getTypeName(writer.writer, v)
    if (v instanceof Binding) { return generateName(writer.writer, v) }
    if (isAst(v) && 'function' in writer) {
      const funcWriter = writer as LlvmFunctionWriter
      const reg = toRegister(funcWriter, writeExpr(funcWriter, v))
      return generateName(writer.writer, reg)
    }
    compilerAssert(false, "Not supported in format", { v, str: String(v) })
  })
  writer.currentOutput.push(s)
}

const globals = {
  printf: new Binding("printf", FunctionType),
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
    mallocBinding: null!
  }
  bytecodeWriter.writer = bytecodeWriter
  bytecodeWriter.currentOutput = bytecodeWriter.outputHeaders

  const insertGlobal = (binding: Binding | Type, name: string) => {
    bytecodeWriter.globalNames.set(binding, name)
    bytecodeWriter.globalNameToBinding.set(name, binding)
  }
  insertGlobal(VoidType, "void")
  insertGlobal(IntType, "i32")
  insertGlobal(BoolType, "i1")
  insertGlobal(FloatType, "float")
  insertGlobal(DoubleType, "double")
  insertGlobal(RawPointerType, "ptr")
  insertGlobal(globals.printf, "@printf")

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


  bytecodeWriter.outputHeaders.push("declare i32 @printf(i8*, ...)\n\n")
  bytecodeWriter.outputHeaders.push(`\n`)

  Array.from(globalCompilerState.compiledFunctions.values()).map(func => {
    generateName(bytecodeWriter, func.binding, true)
    const funcWriter = writeLlvmBytecodeFunction(bytecodeWriter, func)
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
  writer.outputHeaders.push(`${name} = type { `)
  obj.typeInfo.fields.forEach((field, i) => {
    if (i !== 0) writer.outputHeaders.push(', ')
    writer.outputHeaders.push(getTypeName(writer, field.fieldType))
  })
  writer.outputHeaders.push(` }\n`)

  return name
  // compilerAssert(false, "Type not implemented", { obj })
}
const writeLlvmBytecodeFunction = (bytecodeWriter: LlvmWriter, func: CompiledFunction) => {
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
    printNextStatement: false
  }

  const isMain = bytecodeWriter.globalCompilerState.entryFunction === func
  const name = isMain ? "@main" : generateName(bytecodeWriter, func.binding)
  
  funcWriter.currentOutput = funcWriter.outputFunctionBody

  const result = writeExpr(funcWriter, func.body)

  funcWriter.currentOutput = bytecodeWriter.outputStrings

  format(funcWriter, `define $ $(`, func.returnType, name)

  func.argBindings.forEach((binding, i) => {
    if (i !== 0) format(funcWriter, ", ")
    const storageType = binding.storage === 'ref' ? RawPointerType : binding.type
    format(funcWriter, `$ $`, storageType, binding)
  })
  format(funcWriter, `) {\n`, func.returnType, name)

  if (funcWriter.outputFunctionHeaders.length) {
    bytecodeWriter.outputStrings.push(...funcWriter.outputFunctionHeaders)
    bytecodeWriter.outputStrings.push('\n')
  }
  bytecodeWriter.outputStrings.push(...funcWriter.outputFunctionBody)

  if (isMain) { // hardcode for now
    format(funcWriter, `  ret i32 0\n`)
  } else if (func.returnType !== VoidType) {
    const v = toRegister(funcWriter, result)
    format(funcWriter, `  ret $ $\n`, func.returnType, v)
  } else {
    format(funcWriter, `  ret void\n`)
  }
  format(funcWriter, `}\n\n`)

  return funcWriter
};
