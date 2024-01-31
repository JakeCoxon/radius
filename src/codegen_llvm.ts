import { externals, mallocExternal, reallocExternal } from "./compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BoolType, CompiledFunction, ConcreteClassType, DoubleType, FileWriter, FloatType, GlobalCompilerState, IntType, ListTypeConstructor, LlvmFunctionWriter, LlvmWriter, NumberAst, ParameterizedType, PrimitiveType, RawPointerType, StatementsAst, StringType, Type, TypeField, VoidType, compilerAssert, isAst, isType, textColors } from "./defs";

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
  astWriter[ast.key](writer, ast as any);
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

const astWriter: AstWriterTable<LlvmFunctionWriter> = {
  statements: (writer, ast) => {
    // TODO: Filter voids?
    ast.statements.forEach((expr, i) => {
      writer.writer.outputWriter.write("  ; Statement\n")
      writeExpr(writer, expr)
      writer.writer.outputWriter.write("\n")
      compilerAssert(writer.nameStack.length === 0, "Unexpected")
      const toPop = i !== ast.statements.length - 1 || ast.type === VoidType
      if (toPop) {
        writer.valueStack.length = 0 // Ignore remaining values
      }
    })
  },
  string: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  cast: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  binding: (writer, ast) => {
    compilerAssert(ast.binding.type !== VoidType)
    // ast.binding.
    const isArg = !!writer.function.argBindings.find(x => x === ast.binding)
    if (isArg) {
      const name = generateName(writer.writer, ast.binding)
      writer.valueStack.push(`%${name}`)
      return
    }

    const name = `%${generateName(writer.writer, new Binding(`${ast.binding.name}.value`, VoidType))}`
    const ptrName = `%${generateName(writer.writer, ast.binding)}`
    format(writer, "  $ = load $, ptr $\n", name, ast.binding.type, ptrName)
    writer.valueStack.push(name)
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented", { ast })
    
    const ptrName = `%${generateName(writer.writer, ast.binding)}`
    format(writer, "  $ = alloca $\n", ptrName, ast.binding.type)

    const name = `%${generateName(writer.writer, new Binding(`${ast.binding.name}.value`, VoidType))}`
    writer.nameStack.push(name)
    writeExpr(writer, ast.value)

    format(writer, "  store $ $, ptr $\n", ast.binding.type, name, ptrName)
  },
  set: (writer, ast) => {
    const name = `%${generateName(writer.writer, new Binding(`${ast.binding.name}.value`, VoidType))}`
    writer.nameStack.push(name)
    writeExpr(writer, ast.value)

    const ptrName = `%${generateName(writer.writer, ast.binding)}`
    format(writer, "  store $ $, ptr $\n", ast.binding.type, name, ptrName)
  },
  number: (writer, ast) => {
    compilerAssert(ast.type === IntType || ast.type === FloatType || ast.type === DoubleType, "Expected number type got $type", { ast, type: ast.type })
    const name = popNameStackOrGenerateNewName(writer)
    format(writer, `  $ = add $ 0, $ ; literal\n`, name, ast.type, String(ast.value))
    writer.valueStack.push(name)
  },
  bool: (writer, ast) => {
    const name = popNameStackOrGenerateNewName(writer)
    format(writer, `  $ = add $ 0, $ ; literal\n`, name, ast.type, ast.value ? "1" : "0")
    writer.valueStack.push(name)
  },
  if: (writer, ast) => {
    const outName = ast.type !== VoidType ? popNameStackOrGenerateNewName(writer) : undefined
    const thenLabel = generateName(writer.writer, new Binding(`if_then`, VoidType))
    const endLabel = generateName(writer.writer, new Binding(`if_end`, VoidType))
    const elseLabel = ast.falseBody ? generateName(writer.writer, new Binding(`if_else`, VoidType)) : endLabel
    let thenVal: string | undefined = undefined, elseVal: string | undefined = undefined
    let thenFinalLabel: string | undefined = undefined, elseFinalLabel: string | undefined = undefined

    format(writer, `  br i1 $, label %$, label %$\n\n`, ast.expr, thenLabel, elseLabel)
    format(writer, `$:\n`, thenLabel)
    writer.currentBlockLabel = thenLabel
    writeExpr(writer, toStatements(ast.trueBody))
    thenFinalLabel = writer.currentBlockLabel // Nested control flow have have changed current block
    if (outName) thenVal = writer.valueStack.pop()!
    format(writer, `  br label %$\n\n`, endLabel)

    if (ast.falseBody) {
      format(writer, `$:\n`, elseLabel)
      writer.currentBlockLabel = elseLabel
      writeExpr(writer, toStatements(ast.falseBody))
      elseFinalLabel = writer.currentBlockLabel // Nested control flow have have changed current block
      if (outName) elseVal = writer.valueStack.pop()!
      format(writer, `  br label %$\n\n`, endLabel)
    }

    format(writer, `$:\n`, endLabel)
    writer.currentBlockLabel = endLabel
    if (outName) {
      compilerAssert(thenVal && elseVal && elseFinalLabel, "Expected 'then' and 'else' branch")
      format(writer, `  $ = phi $ [ $, %$ ], [ $, %$ ]\n`, outName, ast.type, thenVal, thenFinalLabel, elseVal, elseFinalLabel)
    }
  },
  and: (writer, ast) => {
    const outName = popNameStackOrGenerateNewName(writer)
    const [a, b] = ast.args
    const secondOperand = generateName(writer.writer, new Binding(`and_second_operand`, VoidType))
    const resultFalse = generateName(writer.writer, new Binding(`and_result_false`, VoidType))
    const resultLabel = generateName(writer.writer, new Binding(`and_result`, VoidType))
    format(writer, `  br i1 $, label %$, label %$\n`, a, secondOperand, resultFalse)
    format(writer, `$:\n`, secondOperand)
    writer.currentBlockLabel = secondOperand
    writeExpr(writer, b)
    const secondOperandFinalName = writer.currentBlockLabel // Nested control flow have have changed current block
    const bVal = writer.valueStack.pop()!
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultFalse)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultLabel)
    format(writer, `  $ = phi $ [ false, %$ ], [ $, %$ ]\n`, outName, ast.type, resultFalse, bVal, secondOperandFinalName)
    writer.currentBlockLabel = resultLabel
    writer.valueStack.push(outName)
  },
  or: (writer, ast) => {
    const outName = popNameStackOrGenerateNewName(writer)
    const [a, b] = ast.args
    const secondOperand = generateName(writer.writer, new Binding(`or_second_operand`, VoidType))
    const resultTrue = generateName(writer.writer, new Binding(`or_result_true`, VoidType))
    const resultLabel = generateName(writer.writer, new Binding(`or_result`, VoidType))
    format(writer, `  br i1 $, label %$, label %$\n`, a, resultTrue, secondOperand)
    format(writer, `$:\n`, secondOperand)
    writer.currentBlockLabel = secondOperand
    writeExpr(writer, b)
    const secondOperandFinalName = writer.currentBlockLabel // Nested control flow have have changed current block
    const bVal = writer.valueStack.pop()!
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultTrue)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultLabel)
    format(writer, `  $ = phi $ [ true, %$ ], [ $, %$ ]\n`, outName, ast.type, resultTrue, bVal, secondOperandFinalName)
    writer.currentBlockLabel = resultLabel
    writer.valueStack.push(outName)
  },
  while: (writer, ast) => {
    const loopCondition = generateName(writer.writer, new Binding(`while_condition`, VoidType))
    const loopBody = generateName(writer.writer, new Binding(`while_body`, VoidType))
    const loopEnd = generateName(writer.writer, new Binding(`while_end`, VoidType))

    format(writer, `  br label %$\n`, loopCondition)
    format(writer, `$:\n`, loopCondition)
    writer.currentBlockLabel = loopCondition
    writeExpr(writer, ast.condition)
    const aVal = writer.valueStack.pop()!
    format(writer, `  br i1 $, label %$, label %$\n\n`, aVal, loopBody, loopEnd)

    format(writer, `$:\n`, loopBody)
    writer.currentBlockLabel = loopBody
    writeExpr(writer, ast.body)
    format(writer, `  br label %$\n\n`, loopCondition)

    format(writer, `$:\n`, loopEnd)
    writer.currentBlockLabel = loopEnd
  },

  block: (writer, ast) => {
    writer.blocks.push({ binding: ast.binding }) // not strictly necessary?
    writeExpr(writer, ast.body)
    writer.blocks.pop()!
    const blockEndLabel = generateName(writer.writer, ast.binding)
    format(writer, `  br label %$\n\n`, blockEndLabel)
    format(writer, `$:\n`, blockEndLabel)
    writer.currentBlockLabel = blockEndLabel
  },
  break: (writer, ast) => {
    const block = writer.blocks.findLast(x => x.binding === ast.binding)
    compilerAssert(block, "Programmer error. Expected block") // Programmer error
    format(writer, `  br label %$\n`, ast.binding)
  },
  call: (writer, ast) => {
    if (ast.func.name === "print") {
      compilerAssert(ast.args.length === 1, "Print not implemented yet", { ast });
      const name = popNameStackOrGenerateNewName(writer)
      const formatPtrName = `%${generateName(writer.writer, new Binding(`format_str_ptr`, VoidType))}`
      format(writer, "  $ = getelementptr inbounds [4 x i8], [4 x i8]* @format_string, i64 0, i64 0\n", formatPtrName)
      format(writer, "  $ = call i32 (i8*, ...) @printf(i8* $, $ $)\n", name, formatPtrName, ast.args[0].type, ast.args[0])

      return;
    }

    compilerAssert(false, "Not implemented")
  },
  list: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },

  usercall: (writer, ast) => {
    const name = ast.type !== VoidType ? popNameStackOrGenerateNewName(writer) : undefined

    const argValues = ast.args.map(arg => {
      writeExpr(writer, arg); return writer.valueStack.pop()!
    })
    
    if (name) { format(writer, `  $ = `, name) }
    else { format(writer, `  `) }
    
    format(writer, `call $ @$(`, ast.type, ast.binding)
    ast.args.forEach((arg, i) => {
      if (i !== 0) format(writer, ", ")
      format(writer, `$ $`, arg.type, argValues[i]!)
    })
    format(writer, `)\n`)
    if (name) writer.valueStack.push(name)
  },
  operator: (writer, ast) => {
    const [a, b] = ast.args
    const name = popNameStackOrGenerateNewName(writer)
    compilerAssert(a.type === b.type, "Expected types to be equal", { a, b })
    const op = operatorMap[ast.operator]
    compilerAssert(op, "Expected op", { ast })
    format(writer, `  $ = $ $ $, $\n`, name, op, a.type, a, b)
    writer.valueStack.push(name)
  },
  not: (writer, ast) => {
    const expr = ast.expr
    compilerAssert(expr.type === BoolType, "Expected bool")
    const name = popNameStackOrGenerateNewName(writer)
    format(writer, `  $ = xor $ $, 1\n`, name, expr.type, expr)
    writer.valueStack.push(name)
  },
  defaultcons: (writer, ast) => {
    compilerAssert(false, "Not implemented 'defaultcons'")
  },
  constructor: (writer, ast) => {
    compilerAssert(false, "Not implemented 'constructor'")
  },
  valuefield: (writer, ast) => {
    compilerAssert(false, "Not implemented 'valuefield'")
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
  void: (writer, ast) => {}
};

const format = (writer: LlvmFunctionWriter, format: string, ...args: (string | Type | Ast | Binding)[]) => {
  let i = 0
  const s = format.replace(/\$/g, (x) => {
    const v = args[i++]
    
    if (typeof v === 'string') return v
    if (isType(v)) return getTypeName(writer.writer, v)
    if (v instanceof Binding) { return generateName(writer.writer, v) }
    if (isAst(v)) {
      writeExpr(writer, v)
      compilerAssert(writer.valueStack.length, "Expected value")
      return writer.valueStack.pop()!
    }
    return ''
  })
  writer.writer.outputWriter.write(s)
}
const popNameStackOrGenerateNewName = (writer: LlvmFunctionWriter) => {
  return writer.nameStack.length ? writer.nameStack.pop()! : 
    `%${generateName(writer.writer, new Binding("", VoidType))}`
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
    outputWriter
  }

  bytecodeWriter.globalNames.set(VoidType, "void")
  bytecodeWriter.globalNames.set(IntType, "i32")
  bytecodeWriter.globalNames.set(BoolType, "i1")
  bytecodeWriter.globalNames.set(FloatType, "f32")
  bytecodeWriter.globalNames.set(RawPointerType, "ptr")

  outputWriter.write("declare i32 @printf(i8*, ...)\n\n")
  outputWriter.write(`@format_string = private unnamed_addr constant [4 x i8] c"%i\\0A\\00", align 1\n\n`)

  Array.from(globalCompilerState.compiledFunctions.values()).map(func => {
    const funcWriter = writeLlvmBytecodeFunction(bytecodeWriter, func)
    return funcWriter
  })

  log('\n\n', bytecodeWriter.typeSizes)
  
  return bytecodeWriter
}

const generateName = (writer: LlvmWriter, binding: Binding) => {
  if (writer.globalNames.get(binding)) {
    return writer.globalNames.get(binding)!
  }
  let name = binding.name.replace(/[^a-zA-Z0-9\.]/g, ' ').trim().replace(/ +/g, '_')

  let newName = name; let index = 0
  while (!newName || writer.globalNameToBinding.get(newName)) { newName = `${name}_${index++}` }
  writer.globalNames.set(binding, newName)
  writer.globalNameToBinding.set(newName, binding)
  return newName
}

const getTypeName = (writer: LlvmWriter, obj: Type): string => {
  if (writer.globalNames.get(obj)) {
    return writer.globalNames.get(obj)!
  }
  compilerAssert(false, "Type not implemented", { obj })
}
const writeLlvmBytecodeFunction = (bytecodeWriter: LlvmWriter, func: CompiledFunction) => {
  log("\nWriting func", func.functionDefinition.debugName, "\n")
  const funcWriter: LlvmFunctionWriter = {
    writer: bytecodeWriter,
    function: func,
    nameStack: [],
    valueStack: [],
    currentBlockLabel: '<no name>',
    blocks: []
  }

  const isMain = bytecodeWriter.globalCompilerState.entryFunction === func
  const name = isMain ? "main" : generateName(bytecodeWriter, func.binding)
  
  format(funcWriter, `define $ @$(`, func.returnType, name)
  func.argBindings.forEach((binding, i) => {
    if (i !== 0) format(funcWriter, ", ")
    format(funcWriter, `$ %$`, binding.type, binding)
  })
  format(funcWriter, `) {\n`, func.returnType, name)
  writeExpr(funcWriter, func.body)
  if (func.returnType !== VoidType) {
    compilerAssert(funcWriter.valueStack.length === 1, "Expected 1 value left", { valueStack: funcWriter.valueStack })
    const v = funcWriter.valueStack.pop()!
    format(funcWriter, `  ret $ $\n`, func.returnType, v)
  } else {
    format(funcWriter, `  ret void\n`)
  }
  bytecodeWriter.outputWriter.write(`}\n\n`)

  return funcWriter
};
