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
    const name = getCurrentName(writer)
    format(writer, `  $ = add $ 0, $ ; literal\n`, name, ast.type, String(ast.value))
    writer.valueStack.push(name)
  },
  bool: (writer, ast) => {
    const name = getCurrentName(writer)
    format(writer, `  $ = add $ 0, $ ; literal\n`, name, ast.type, ast.value ? "1" : "0")
    writer.valueStack.push(name)
  },
  if: (writer, ast) => {
    const outName = ast.type !== VoidType ? getCurrentName(writer) : undefined
    const thenLabel = generateName(writer.writer, new Binding(`if_then`, VoidType))
    const endLabel = generateName(writer.writer, new Binding(`if_end`, VoidType))
    const elseLabel = ast.falseBody ? generateName(writer.writer, new Binding(`if_else`, VoidType)) : endLabel
    let thenVal: string | undefined = undefined, elseVal: string | undefined = undefined

    format(writer, `  br i1 $, label %$, label %$\n\n`, ast.expr, thenLabel, elseLabel)
    format(writer, `$:\n`, thenLabel)
    writeExpr(writer, toStatements(ast.trueBody))
    if (outName) thenVal = writer.valueStack.pop()!
    format(writer, `  br label %$\n\n`, endLabel)

    if (ast.falseBody) {
      format(writer, `$:\n`, elseLabel)
      writeExpr(writer, toStatements(ast.falseBody))
      if (outName) elseVal = writer.valueStack.pop()!
      format(writer, `  br label %$\n\n`, endLabel)
    }
    format(writer, `$:\n`, endLabel)
    if (outName) {
      compilerAssert(thenVal && elseVal, "Expected 'then' and 'else' branch")
      format(writer, `  $ = phi $ [ $, %$ ], [ $, %$ ]\n`, outName, ast.type, thenVal, thenLabel, elseVal, elseLabel)
    }
  },
  while: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  break: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  and: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  or: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  call: (writer, ast) => {
    if (ast.func.name === "print") {
      compilerAssert(ast.args.length === 1, "Print not implemented yet", { ast });
      const name = getCurrentName(writer)
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
    const name = ast.type !== VoidType ? getCurrentName(writer) : undefined

    const argValues = ast.args.map(arg => {
      writeExpr(writer, arg); return writer.valueStack.pop()!
    })
    
    if (name) { format(writer, `  $ = `, name) }
    else { format(writer, `  `) }
    
    format(writer, `call $ @$(`, ast.type, ast.binding)
    ast.args.forEach((arg, i) => {
      if (i !== 0) format(writer, ", ")
      format(writer, `$ $`, arg.type, argValues.pop()!)
    })
    format(writer, `)\n`)
    if (name) writer.valueStack.push(name)
  },
  operator: (writer, ast) => {
    const [a, b] = ast.args
    const name = getCurrentName(writer)
    compilerAssert(a.type === b.type, "Expected types to be equal", { a, b })
    const op = operatorMap[ast.operator]
    compilerAssert(op, "Expected op", { ast })
    format(writer, `  $ = $ $ $, $\n`, name, op, a.type, a, b)
    writer.valueStack.push(name)
  },
  not: (writer, ast) => {
    const expr = ast.expr
    compilerAssert(expr.type === BoolType, "Expected bool")
    const name = getCurrentName(writer)
    format(writer, `  $ = xor $ $, 1\n`, name, expr.type, expr)
    writer.valueStack.push(name)
  },
  defaultcons: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  constructor: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  valuefield: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  field: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  setvaluefield: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  setfield: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  subscript: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  setsubscript: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  deref: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  setderef: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  block: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  return: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  address: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  void: (writer, ast) => {}
};

const format = (writer: LlvmFunctionWriter, format: string, ...args: (string | Type | Ast | Binding)[]) => {
  let i = 0
  const s = format.replace(/\$/g, (x) => {
    const v = args[i++]
    
    if (typeof v === 'string') return v
    if (isType(v)) return getName(writer.writer, v)
    if (isAst(v)) {
      writeExpr(writer, v)
      compilerAssert(writer.valueStack.length, "Expected value")
      return writer.valueStack.pop()!
    }
    if (v instanceof Binding) {
      return generateName(writer.writer, v)
    }
    return ''
  })
  writer.writer.outputWriter.write(s)
}
const getCurrentName = (writer: LlvmFunctionWriter) => {
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

const getName = (writer: LlvmWriter, obj: Binding | Type): string => {
  if (writer.globalNames.get(obj)) {
    return writer.globalNames.get(obj)!
  }
  if (obj instanceof Binding) {
    return generateName(writer, obj)
  }
  compilerAssert(false, "Object not found", { obj })
}
const writeLlvmBytecodeFunction = (bytecodeWriter: LlvmWriter, func: CompiledFunction) => {
  log("\nWriting func", func.functionDefinition.debugName, "\n")
  const funcWriter: LlvmFunctionWriter = {
    writer: bytecodeWriter,
    function: func,
    nameStack: [],
    valueStack: [],
  }

  const isMain = bytecodeWriter.globalCompilerState.entryFunction === func
  const name = isMain ? "main" : generateName(bytecodeWriter, func.binding)
  
  // bytecodeWriter.outputWriter.write(`define ${getName(bytecodeWriter, func.returnType)} @${name}(i32 %a, i32 %b) {\n`)
  format(funcWriter, `define $ @$(`, func.returnType, name)
  func.argBindings.forEach((binding, i) => {
    if (i !== 0) format(funcWriter, ", ")
    format(funcWriter, `$ %$`, binding.type, binding)
  })
  format(funcWriter, `) {\n`, func.returnType, name)
  writeExpr(funcWriter, func.body)
  if (func.returnType !== VoidType) {
    compilerAssert(funcWriter.valueStack.length === 1, "Expected 1 value left")
    const v = funcWriter.valueStack.pop()!
    format(funcWriter, `  ret $ $\n`, func.returnType, v)
  } else {
    format(funcWriter, `  ret void\n`)
  }
  bytecodeWriter.outputWriter.write(`}\n\n`)

  return funcWriter
};
