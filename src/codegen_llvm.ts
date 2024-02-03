import { externals, mallocExternal, reallocExternal } from "./compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BoolType, CompiledFunction, ConcreteClassType, DoubleType, FileWriter, FloatType, FunctionType, GlobalCompilerState, IntType, ListTypeConstructor, LlvmFunctionWriter, LlvmWriter, NumberAst, ParameterizedType, PrimitiveType, RawPointerType, StatementsAst, StringType, Type, TypeField, ValueFieldAst, VoidType, compilerAssert, isAst, isType, textColors } from "./defs";

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
  "/": "div",
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
  return astWriter[ast.key](writer, ast as any)
};

const constantTableByType = (writer: LlvmFunctionWriter, type: Type) => {
  if (type === RawPointerType || type === BoolType) type = IntType // normalise int types
  let byType = writer.constantsByType.get(type)
  if (!byType) { byType = new Map(); writer.constantsByType.set(type, byType) }
  return byType
}
const emitConstant = (writer: LlvmFunctionWriter, type: Type, value: number) => {
  compilerAssert(false, "Not implemented")
}
const toStatements = (ast: Ast) => {
  if (ast instanceof StatementsAst) return ast
  return new StatementsAst(ast.type, ast.location, [ast])
}

const toRegister = (writer: LlvmFunctionWriter, v: LlvmResultValue): Register => {
  compilerAssert(v)
  if ('register' in v) return v.register
  const name = createRegister("", VoidType)
  format(writer, "  $ = load $, ptr $\n", name, v.type, v.pointer)
  return name
}
const getPointerName = (writer: LlvmFunctionWriter, type: Type) => {
  if (type === RawPointerType) return 'ptr'
  return `${getTypeName(writer.writer, type)}*`
}

export type LlvmResultValue = 
  { register: Register } | 
  { pointer: Pointer, type: Type } |
  null

type Pointer = Binding & {_type: 'pointer'}
type Register = Binding & {_type: 'register'}

const createPointer = (name: string, type: Type) => new Binding(name, type) as Pointer
const createRegister = (name: string, type: Type) => new Binding(name, type) as Register


export type LlvmAstWriterTable = {
  [A in Ast as A['key']]: (writer: LlvmFunctionWriter, ast: A) => LlvmResultValue;
}

const beginBasicBlock = (writer: LlvmFunctionWriter, label: Binding) => {
  format(writer, "$:\n", generateName(writer.writer, label).substring(1))
  writer.currentBlockLabel = label
}

const astWriter: LlvmAstWriterTable = {
  statements: (writer, ast) => {
    // TODO: Filter voids?
    let result: LlvmResultValue = undefined!
    ast.statements.forEach((expr, i) => {
      writer.currentOutput.push("; Statement\n")
      result = writeExpr(writer, expr)
      writer.currentOutput.push("\n")
    })
    return result
  },
  string: (writer, ast) => {
    const constantName = generateName(writer.writer, new Binding("constant", VoidType), true)
    const escaped = ast.value.replace(/["\\]|[\x00-\x1F\x80-\xFF]/g, (str) => {
      return `\\${str.charCodeAt(0).toString(16)}`
    })
    const length = ast.value.length + 1 // add null terminator
    writer.writer.outputHeaders.push(`${constantName} = private unnamed_addr constant [${length} x i8] c"${escaped}\\00"\n`)
    const name = createPointer("", VoidType)
    writer.currentOutput = writer.outputFunctionHeaders
    format(writer, "  $ = alloca $\n", name, ast.type)
    writer.currentOutput = writer.outputFunctionBody

    format(writer, `  store $ { i32 $, ptr $ }, ptr $\n`, ast.type, String(length - 1), constantName, name)
    return { pointer: name, type: ast.type }
  },
  cast: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  binding: (writer, ast) => {
    compilerAssert(ast.binding.type !== VoidType)
    const isArg = !!writer.function.argBindings.find(x => x === ast.binding)
    if (isArg) return { register: ast.binding as Register }
    return { pointer: ast.binding as Pointer, type: ast.type }
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented", { ast })
    const ptrName = ast.binding as Pointer
    writer.currentOutput = writer.outputFunctionHeaders
    format(writer, "  $ = alloca $\n", ptrName, ast.binding.type)
    writer.currentOutput = writer.outputFunctionBody
    const result = toRegister(writer, writeExpr(writer, ast.value))
    format(writer, "  store $ $, ptr $\n", ast.binding.type, result, ptrName)
    return null
  },
  set: (writer, ast) => {
    const result = toRegister(writer, writeExpr(writer, ast.value))
    const ptrName = ast.binding
    format(writer, "  store $ $, ptr $\n", ast.binding.type, result, ptrName)
    return null
  },
  number: (writer, ast) => {
    compilerAssert(ast.type === IntType || ast.type === FloatType || ast.type === DoubleType, "Expected number type got $type", { ast, type: ast.type })
    const ptrName = createPointer("", VoidType)
    const valueName = createRegister("", VoidType)
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
    if (ast.func.name === "print") {
      compilerAssert(ast.args.length === 1, "Print not implemented yet", { ast });
      const name = createRegister("", VoidType)
      const formatPtrName = ast.args[0].type === IntType ? globals.format_string_int :
        ast.args[0].type === FloatType ? globals.format_string_float :
        ast.args[0].type === RawPointerType ? globals.format_string_ptr :
        ast.args[0].type === StringType ? globals.format_string_string : undefined
      compilerAssert(formatPtrName, "Not implemented for this type", { type: ast.args[0].type })
      const argName = toRegister(writer, writeExpr(writer, ast.args[0]))

      if (ast.args[0].type === StringType) {
        const fields = ast.args[0].type.typeInfo.fields

        const loadField = (basePointer: Pointer, structType: Type, field: TypeField) => {
          const fieldType = field.fieldType
          const loadFieldPtr = createPointer("", VoidType)
          const loadFieldVal = createRegister("", field.fieldType)
          format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, structType, getPointerName(writer, structType), basePointer, String(field.index))
          format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
          return loadFieldVal
        }
        const basePointer = createPointer("", VoidType)
        writer.currentOutput = writer.outputFunctionHeaders
        format(writer, "  $ = alloca $\n", basePointer, StringType)
        writer.currentOutput = writer.outputFunctionBody
        format(writer, "  store $ $, ptr $\n", StringType, argName, basePointer)
        const lengthRegister = loadField(basePointer, StringType, fields.find(x => x.name === 'length')!)
        const strPtrRegister = loadField(basePointer, StringType, fields.find(x => x.name === 'data')!)
        format(writer, "  $ = call i32 (i8*, ...) @printf(ptr $, $ $, $ $)\n", name, formatPtrName, lengthRegister.type, lengthRegister, strPtrRegister.type, strPtrRegister)
      } else {
        format(writer, "  $ = call i32 (i8*, ...) @printf(ptr $, $ $)\n", name, formatPtrName, ast.args[0].type, argName)
      }
      return { register: name }
    }

    compilerAssert(false, "Not implemented")
  },
  list: (writer, ast) => {
    compilerAssert(false, "Not implemented")
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
  defaultcons: (writer, ast) => {
    compilerAssert(false, "Not implemented 'defaultcons'")
  },
  constructor: (writer, ast) => {
    const structPtr = createPointer("", VoidType)
    writer.currentOutput = writer.outputFunctionHeaders
    format(writer, "  $ = alloca $\n", structPtr, ast.type)
    writer.currentOutput = writer.outputFunctionBody

    ast.args.forEach((arg, index) => {
      const reg = toRegister(writer, writeExpr(writer, arg))

      const fieldPtr = createPointer('', VoidType)
      const field = ast.type.typeInfo.fields[index]

      format(writer, "  $ = getelementptr $, ptr $, i32 0, i32 $\n", fieldPtr, ast.type, structPtr, String(index))
      format(writer, "  store $ $, $ $\n", field.fieldType, reg, getPointerName(writer, field.fieldType), fieldPtr)
    })

    return { pointer: structPtr, type: ast.type }
  },
  valuefield: (writer, ast) => {

    const loadField = (base: LlvmResultValue, field: TypeField): LlvmResultValue => {
      compilerAssert(base, "")
      const fieldType = field.fieldType
      
      const loadFieldPtr = createPointer("", VoidType)
      const loadFieldVal = createRegister("", VoidType)

      if ('pointer' in base) {
        format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, field.sourceType, getPointerName(writer, field.sourceType), base.pointer, String(field.index))
        format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
      } else {
        format(writer, "  $ = extractvalue $ $, $\n", loadFieldVal, field.sourceType, base.register, String(field.index))
      }
      return { register: loadFieldVal }
    }
    const leftResult = writeExpr(writer, ast.left)
    const reg = ast.fieldPath.reduce((reg, field) => {
      return loadField(leftResult, ast.fieldPath[0])
    }, leftResult)

    return reg
  },
  field: (writer, ast) => {
    compilerAssert(false, "Not implemented 'field'")
  },
  setvaluefield: (writer, ast) => {
    compilerAssert(false, "Not implemented 'setvaluefield'")
  },
  setfield: (writer, ast) => {
    compilerAssert(false, "Not implemented 'setfield'")
  },
  subscript: (writer, ast) => {
    compilerAssert(false, "Not implemented 'subscript'")
  },
  setsubscript: (writer, ast) => {
    compilerAssert(false, "Not implemented 'setsubscript'")
  },
  deref: (writer, ast) => {
    compilerAssert(false, "Not implemented 'deref'")
  },
  setderef: (writer, ast) => {
    compilerAssert(false, "Not implemented 'setderef'")
  },
  return: (writer, ast) => {
    compilerAssert(false, "Not implemented 'return'")
  },
  address: (writer, ast) => {
    compilerAssert(false, "Not implemented 'address'")
  },
  void: (writer, ast) => {
    return null
  }
};

const format = (writer: LlvmFunctionWriter, format: string, ...args: (string | number | Type | Ast | Binding)[]) => {
  let i = 0
  const s = format.replace(/\$/g, (x) => {
    const v = args[i++]
    
    if (typeof v === 'string') return v
    if (typeof v === 'number') return String(v)
    if (isType(v)) return getTypeName(writer.writer, v)
    if (v instanceof Binding) { return generateName(writer.writer, v) }
    if (isAst(v)) {
      const reg = toRegister(writer, writeExpr(writer, v))
      return generateName(writer.writer, reg)
    }
    compilerAssert(false, "Not supported in format", { v, str: String(v) })
  })
  writer.currentOutput.push(s)
}

const globals = {
  printf: new Binding("printf", FunctionType),
  format_string_int: new Binding("format_string_int", RawPointerType),
  format_string_float: new Binding("format_string_float", RawPointerType),
  format_string_ptr: new Binding("format_string_ptr", RawPointerType),
  format_string_string: new Binding("format_string_string", RawPointerType),
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
  }

  const insertGlobal = (binding: Binding | Type, name: string) => {
    bytecodeWriter.globalNames.set(binding, name)
    bytecodeWriter.globalNameToBinding.set(name, binding)
  }
  insertGlobal(VoidType, "void")
  insertGlobal(IntType, "i32")
  insertGlobal(BoolType, "i1")
  insertGlobal(FloatType, "f32")
  insertGlobal(RawPointerType, "ptr")
  insertGlobal(globals.printf, "@printf")

  insertGlobal(globals.format_string_int, "@format_string_int")
  insertGlobal(globals.format_string_float, "@format_string_float")
  insertGlobal(globals.format_string_ptr, "@format_string_ptr")
  insertGlobal(globals.format_string_string, "@format_string_string")

  bytecodeWriter.outputHeaders.push("declare i32 @printf(i8*, ...)\n\n")
  bytecodeWriter.outputHeaders.push(`@format_string_int = private unnamed_addr constant [4 x i8] c"%i\\0A\\00", align 1\n`)
  bytecodeWriter.outputHeaders.push(`@format_string_float = private unnamed_addr constant [4 x i8] c"%f\\0A\\00", align 1\n`)
  bytecodeWriter.outputHeaders.push(`@format_string_ptr = private unnamed_addr constant [4 x i8] c"%p\\0A\\00", align 1\n`)
  bytecodeWriter.outputHeaders.push(`@format_string_string = private unnamed_addr constant [6 x i8] c"%.*s\\0A\\00", align 1\n`)
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
    currentOutput: null!
  }

  const isMain = bytecodeWriter.globalCompilerState.entryFunction === func
  const name = isMain ? "@main" : generateName(bytecodeWriter, func.binding)
  
  funcWriter.currentOutput = funcWriter.outputFunctionBody

  const result = writeExpr(funcWriter, func.body)

  funcWriter.currentOutput = bytecodeWriter.outputStrings

  format(funcWriter, `define $ $(`, func.returnType, name)

  func.argBindings.forEach((binding, i) => {
    if (i !== 0) format(funcWriter, ", ")
    format(funcWriter, `$ $`, binding.type, binding)
  })
  format(funcWriter, `) {\n`, func.returnType, name)

  if (funcWriter.outputFunctionHeaders.length) {
    bytecodeWriter.outputStrings.push(...funcWriter.outputFunctionHeaders)
    bytecodeWriter.outputStrings.push('\n')
  }
  bytecodeWriter.outputStrings.push(...funcWriter.outputFunctionBody)

  if (func.returnType !== VoidType) {
    const v = toRegister(funcWriter, result)
    format(funcWriter, `  ret $ $\n`, func.returnType, v)
  } else {
    format(funcWriter, `  ret void\n`)
  }
  format(funcWriter, `}\n\n`)

  return funcWriter
};
