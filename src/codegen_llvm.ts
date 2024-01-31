import { externals, mallocExternal, reallocExternal } from "./compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BoolType, CompiledFunction, ConcreteClassType, DoubleType, FileWriter, FloatType, GlobalCompilerState, IntType, ListTypeConstructor, LlvmFunctionWriter, LlvmWriter, NumberAst, ParameterizedType, PrimitiveType, RawPointerType, StringType, Type, TypeField, VoidType, compilerAssert, isAst, isType, textColors } from "./defs";

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
  // "==": "Equal",
  // ">": "Greater",
  // "<": "Less",
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


const astWriter: AstWriterTable<LlvmFunctionWriter> = {
  statements: (writer, ast) => {
    writer.currentScopeIndex ++
    // TODO: Filter voids?
    ast.statements.forEach((expr, i) => {
      writer.writer.outputWriter.write("  ; Statement\n")
      writeExpr(writer, expr)
      writer.writer.outputWriter.write("\n")
      compilerAssert(writer.nameStack.length === 0, "Unexpected")
      writer.valueStack.length = 0 // Ignore remaining values
      
    })
  },
  string: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  cast: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  binding: (writer, ast) => {
    compilerAssert(ast.binding.type !== VoidType);
    const name = generateName(writer.writer, ast.binding)
    writer.valueStack.push(`%${name}`)
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented", { ast })

    const name = generateName(writer.writer, ast.binding)
    writer.nameStack.push(`%${name}`)
    
    writeExpr(writer, ast.value)
  },
  set: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  number: (writer, ast) => {
    compilerAssert(ast.type === IntType || ast.type === FloatType || ast.type === DoubleType, "Expected number type got $type", { ast, type: ast.type })
    const name = getCurrentName(writer)
    format(writer, `  $ = add $ 0, $ ; literal\n`,name, ast.type, String(ast.value))
    writer.valueStack.push(name)
  },
  bool: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  if: (writer, ast) => {
    compilerAssert(false, "Not implemented")
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
    const [a, b] = ast.args;
    const name = getCurrentName(writer)
    const op = operatorMap[ast.operator]
    compilerAssert(op, "Expected op", { ast })
    format(writer, `  $ = $ $ $, $\n`, name, op, ast.type, a, b)
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
  not: (writer, ast) => {
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
  let name = binding.name.replace(/[^a-zA-Z0-9]/g, ' ').trim().replace(/ +/g, '_')

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
    argSlots: 0,
    returnSlots: 0,
    bytecode: [],
    constantsByType: new Map(),
    constantSlots: [],
    nextConstantSlot: 0,
    blocks: [],
    locals: [],
    currentScopeIndex: 0,
    nextLocalSlot: 0,
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
