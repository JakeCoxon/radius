import { externalBuiltinBindings } from "./compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BlockAst, BoolType, CallAst, CompiledFunction, ConcreteClassType, ConstructorAst, DefaultConsAst, DoubleType, FileWriter, FloatType, FunctionType, GlobalCompilerState, IntType, ListTypeConstructor, LlvmFunctionWriter, LlvmWriter, NeverType, NumberAst, ParameterizedType, Pointer, PrimitiveType, RawPointerType, Register, SourceLocation, StatementsAst, StringType, Type, TypeField, UserCallAst, ValueFieldAst, VoidType, compilerAssert, isAst, isType, textColors, u64Type, u8Type } from "./defs";

const log = (...args: any[]) => {
  if ((globalThis as any).logger) (globalThis as any).logger.log(...args)
}

const writeExpr = (writer: SyntaxWriter, ast: Ast) => {
  compilerAssert(!writer.writer.astVisitMap.has(ast), "Already visited AST", { ast }) 
  if (!(ast instanceof NumberAst || ast instanceof BindingAst)) {
    writer.writer.astVisitMap.set(ast, true) // Takes up a lot of memory
  }
  compilerAssert(astWriter[ast.key], `Not implemented ast writer '${ast.key}'`, { ast })
  const toPrint = !(ast instanceof StatementsAst || ast instanceof BlockAst) && writer.printNextStatement
  if (toPrint) {
    writer.printNextStatement = false
    // const line = ast.location.source?.input.split("\n").find((x, i) => i + 1 === ast.location.line)
    // format(writer, "  ".repeat(writer.indent))
    // if (line) {
    //   format(writer, "# $ | ($) $\n", ast.location.line, ast.key, line.trim())
    // } else format(writer, `# $\n`, ast.key)
    format(writer, "  ".repeat(writer.indent))
  }
  const result = astWriter[ast.key](writer, ast as any)
  if (toPrint) format(writer, "\n")
  return result
};

type RegisterResult = { register: Register }
type PointerResult = { pointer: Pointer }
export type LlvmResultValue = RegisterResult | PointerResult | null

type SyntaxWriter = LlvmFunctionWriter & {
  indent: number
}

export type SyntaxAstWriterTable = {
  [A in Ast as A['key']]: (writer: SyntaxWriter, ast: A) => void;
}

const toStmts = (ast: Ast) => {
  if (ast instanceof StatementsAst) return ast
  return new StatementsAst(ast.type, ast.location, [ast])
}

const astWriter: SyntaxAstWriterTable = {
  statements: (writer, ast) => {
    // TODO: Filter voids?
    let result: LlvmResultValue = undefined!
    // format(writer, "  ".repeat(writer.indent))
    format(writer, "{\n")
    writer.indent ++
    const visit = (expr: Ast) => {
      if (expr instanceof StatementsAst) {
        expr.statements.forEach(visit)
        // compilerAssert(false, "Not implemented")
        return
      }
      if (expr instanceof BlockAst) {
        visit(expr.body)
        return
      }

      writer.printNextStatement = true
      writeExpr(writer, expr)
    }
    ast.statements.forEach(visit)
    writer.indent --
    format(writer, "  ".repeat(writer.indent))
    format(writer, "}")
  },
  string: (writer, ast) => {
    const str = ast.value.replace(/["\\]|[\x00-\x1F\x80-\xFF]/g, (str) => {
      if (str === '"') return '\\"'
      if (str === '\\') return '\\\\'
      if (str === '\n') return '\\n'
      return `\\${str.charCodeAt(0).toString(16).padStart(2, '0')}`
    })
    format(writer, `"${str}"`)
  },
  binding: (writer, ast) => {
    compilerAssert(ast.binding.type !== VoidType)
    format(writer, `$`, ast.binding)
  },
  let: (writer, ast) => {
    compilerAssert(ast.value)
    format(writer, "let $ = $", ast.binding, ast.value)
  },
  set: (writer, ast) => {
    format(writer, "$ = $", ast.binding, ast.value)
  },
  number: (writer, ast) => {
    format(writer, `$`, ast.value)
  },
  bool: (writer, ast) => {
    format(writer, `$`, ast.value ? "true" : "false")
  },
  if: (writer, ast) => {
    if (ast.falseBody)
      format(writer, "if $ $ else $", ast.expr, toStmts(ast.trueBody), toStmts(ast.falseBody))
    else 
      format(writer, "if $ $", ast.expr, toStmts(ast.trueBody))
  },
  and: (writer, ast) => {
    format(writer, `($ and $)`, ast.args[0], ast.args[1])
  },
  or: (writer, ast) => {
    format(writer, `($ or $)`, ast.args[0], ast.args[1])
  },
  while: (writer, ast) => {
    format(writer, "while $ $", ast.condition, toStmts(ast.body))
  },

  interleave: (writer, ast) => {
    format(writer, "interleave '$ $ else $", ast.binding, toStmts(ast.entryBlock), toStmts(ast.elseBlock))
  },
  continueinter: (writer, ast) => {
    format(writer, "continueinter '$", ast.interleaveBinding)
  },

  block: (writer, ast) => {
    writeExpr(writer, ast.body)
  },
  break: (writer, ast) => {
    format(writer, "break")
  },
  call: (writer, ast) => {
    format(writer, `$(`, ast.binding)
    ast.args.forEach((arg, i) => {
      writeExpr(writer, arg)
      if (i < ast.args.length - 1) format(writer, ", ")
    })
    format(writer, `)`)
  },

  usercall: (writer, ast) => {

    format(writer, `$(`, ast.binding)
    ast.args.forEach((arg, i) => {
      writeExpr(writer, arg)
      if (i < ast.args.length - 1) format(writer, ", ")
    })
    format(writer, `)`)
  },
  operator: (writer, ast) => {
    format(writer, `($ $ $)`, ast.args[0], ast.operator, ast.args[1])
  },
  not: (writer, ast) => {
    format(writer, `!$`, ast.expr)
  },
  constructor: (writer, ast) => {
    format(writer, `$(`, ast.type)
    ast.args.forEach((arg, i) => {
      writeExpr(writer, arg)
      if (i < ast.args.length - 1) format(writer, ", ")
    })
    format(writer, `)`)

  },
  valuefield: (writer, ast) => {
    format(writer, `$`, ast.left)
    ast.fieldPath.forEach(field => {
      format(writer, `.$`, field.name)
    })
  },
  field: (writer, ast) => {
    format(writer, `field not implemented`)
    // if (!ast.left.type.typeInfo.isReferenceType) {
    //   // This is just valuefield behaviour
    //   const leftResult = writeExpr(writer, ast.left)
    //   const reg = loadFieldHelper(writer, leftResult, [ast.field])
    //   return { register: reg }
    // }
    // const leftResult = toRegister(writer, writeExpr(writer, ast.left))
    // const register = createRegister("", ast.left.type)
    // const dataType = getDataTypeName(writer.writer, register.type)
    // format(writer, "  $ = load $, ptr $\n", register, dataType, leftResult)
    // const reg = loadFieldHelper(writer, { register }, [ast.field])
    // return { register: reg }
  },
  setvaluefield: (writer, ast) => {
    format(writer, `$`, ast.left)
    ast.fieldPath.forEach(field => {
      format(writer, `.$`, field.name)
    })
    format(writer, ` = $`, ast.value)
  },
  setfield: (writer, ast) => {
    format(writer, `setfield not implemented`)
    // const reg = toRegister(writer, writeExpr(writer, ast.left))
    // const leftPtr = reg as unknown as Pointer // reinterpret reg as a pointer
    // const fieldPtr = getElementPointer(writer, leftPtr, leftPtr.type, [ast.field])
    // const valueReg = toRegister(writer, writeExpr(writer, ast.value))
    // format(writer, "  store $ $, $ $\n", ast.field.fieldType, valueReg, 'ptr', fieldPtr)
  },
  cast: (writer, ast) => {
    format(writer, `$ as $`, ast.expr, ast.type)
  },
  defaultcons: (writer, ast) => {
    
    if (ast.type === IntType || ast.type === FloatType || ast.type === DoubleType || ast.type === RawPointerType) {
      return writeExpr(writer, new NumberAst(ast.type, ast.location, 0))
    }
    // format(writer, `defaultcons not implemented`)
    const fields = ast.type.typeInfo.fields.map(x => new DefaultConsAst(x.fieldType, ast.location))
    return writeExpr(writer, new ConstructorAst(ast.type, ast.location, fields))
  },
  list: (writer, ast) => {
    compilerAssert(false, "Not implemented 'list'", { ast })
  },
  subscript: (writer, ast) => {
    format(writer, `$[$]`, ast.left, ast.right)
  },
  setsubscript: (writer, ast) => {
    format(writer, `$[$] = $`, ast.left, ast.right, ast.value)
  },
  deref: (writer, ast) => {
    format(writer, `$`, ast.left)
    ast.fieldPath.forEach(field => {
      format(writer, `.$`, field.name)
    })
  },
  setderef: (writer, ast) => {
    format(writer, `$`, ast.left)
    ast.fieldPath.forEach(field => {
      format(writer, `.$`, field.name)
    })
    format(writer, ` = $`, ast.value)
  },
  return: (writer, ast) => {
    if (!ast.expr) format(writer, "return")
    else format(writer, "return $", ast.expr)
  },
  address: (writer, ast) => {
    format(writer, `&$`, ast.binding)
    // return { register: ast.binding as Register }

  },
  void: (writer, ast) => {
    format(writer, `# nothing`)
  },
  comptimeobj: (writer, ast) => {
    compilerAssert(false, "Error unexpected 'comptimeobj'", { ast })
  },
  namedarg: (writer, ast) => {
    compilerAssert(false, "Error unexpected 'namedarg'", { ast })
  }
};

type Writable = { writer: LlvmWriter, currentOutput: string[] }
const format = (writer: Writable, format: string, ...args: (string | number | Type | Ast | Binding)[]) => {
  let i = 0
  const write = (v: unknown) => {
    if (typeof v === 'string') return v
    if (typeof v === 'number') return String(v)
    if (isType(v)) return getTypeName(writer.writer, v)
    if (v instanceof Binding) { return generateName(writer.writer, v) }
    if (isAst(v) && 'function' in writer) {
      const funcWriter = writer as SyntaxWriter
      writeExpr(funcWriter, v)
      return ''
    }
    if (typeof v === 'undefined') return 'undefined'
    compilerAssert(false, "Not supported in format", { v, str: String(v) })
  }
  const splits = format.split(/\$/)
  splits.forEach((split, i) => {
    writer.currentOutput.push(split)
    if (i < splits.length - 1) {
      const x = write(args[i])
      if (x) writer.currentOutput.push(x)
    }
    
    
  })
}

export const writeSyntax = (globalCompilerState: GlobalCompilerState, outputWriter: FileWriter) => {
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
    astVisitMap: new Map()
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

    const defaultValueLiteral = (type: Type): string => {
      if (type === FloatType) return '0.0'
      else if (type === DoubleType) return '0.0'
      else if (type === IntType || type === u64Type) return '0'
      else if (type === RawPointerType) return 'null'
      else if (type.typeInfo.isReferenceType) return 'null'
      else {
        const fields = type.typeInfo.fields.map(x => 
          `${getTypeName(bytecodeWriter, x.fieldType)} ${defaultValueLiteral(x.fieldType)}`)
        return `{ ${fields.join(", ")} }`
      }
    }
    
    format(bytecodeWriter, "$ = global $ $\n", name, globalLet.binding.type, defaultValueLiteral(globalLet.binding.type))
  })
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
  if (global) name = `${name}`
  else name = `${name}`

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
  const name = generateName(writer, new Binding(`%${obj.shortName}`, VoidType))

  writer.globalNames.set(obj, name)
  format(writer, `type $ {\n`, name)
  obj.typeInfo.fields.forEach((field, i) => {
    format(writer, "  $: $\n", field.name, field.fieldType)
  })
  writer.outputHeaders.push(`}\n`)

  return name
  // compilerAssert(false, "Type not implemented", { obj })
}
const writeLlvmBytecodeFunction = (bytecodeWriter: LlvmWriter, func: CompiledFunction) => {
  log("\nWriting func", func.functionDefinition.debugName, "\n")
  const funcWriter: SyntaxWriter = {
    writer: bytecodeWriter,
    function: func,
    currentBlockLabel: null!,
    blocks: [],
    constantsByType: new Map(),
    outputFunctionBody: [],
    outputFunctionHeaders: [],
    currentOutput: null!,
    printNextStatement: false,
    indent: 0
  }

  const isMain = bytecodeWriter.globalCompilerState.entryFunction === func
  const name = isMain ? "@main" : generateName(bytecodeWriter, func.binding)
  
  funcWriter.currentOutput = funcWriter.outputFunctionBody

  const result = writeExpr(funcWriter, func.body)

  funcWriter.currentOutput = bytecodeWriter.outputStrings

  format(funcWriter, `fn $(`, name)

  func.argBindings.forEach((binding, i) => {
    if (i !== 0) format(funcWriter, ", ")
    const storageType = binding.storage === 'ref' ? RawPointerType : binding.type
    generateName(bytecodeWriter, binding)
  
    const argValueBinding = new Binding(binding.name, binding.type)
    format(funcWriter, `$ $`, storageType, argValueBinding)
    
    // allocaHelper(funcWriter, binding as Pointer)
    funcWriter.currentOutput = funcWriter.outputFunctionHeaders
    // format(funcWriter, "  store $ $, $ $\n", storageType, argValueBinding, 'ptr', binding)
    funcWriter.currentOutput = bytecodeWriter.outputStrings
  })
  format(funcWriter, `) -> $ `, func.returnType)

  if (funcWriter.outputFunctionHeaders.length) {
    bytecodeWriter.outputStrings.push(...funcWriter.outputFunctionHeaders)
    bytecodeWriter.outputStrings.push('\n')
  }
  bytecodeWriter.outputStrings.push(...funcWriter.outputFunctionBody)

  if (isMain) { // hardcode for now
    // format(funcWriter, `  ret i32 0\n`)
  } else if (func.body.type === NeverType) {
    // format(funcWriter, `  unreachable\n`)
  } else if (func.returnType !== VoidType) {
    // const v = toRegister(funcWriter, result)
    // format(funcWriter, `  ret $ $\n`, func.returnType, v)
  } else {
    // format(funcWriter, `  ret void\n`)
  }
  format(funcWriter, `\n\n`)

  return funcWriter
};
