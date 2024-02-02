import { externals, mallocExternal, reallocExternal } from "./compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BoolType, CompiledFunction, ConcreteClassType, DoubleType, FileWriter, FloatType, GlobalCompilerState, IntType, ListTypeConstructor, LlvmFunctionWriter, LlvmResultValue, LlvmWriter, NumberAst, ParameterizedType, PrimitiveType, RawPointerType, StatementsAst, StringType, Type, TypeField, ValueFieldAst, VoidType, compilerAssert, isAst, isType, textColors } from "./defs";

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

const writeExpr = (writer: LlvmFunctionWriter, ast: Ast, opts?: Partial<NodeInfo>) => {
  compilerAssert(false, `Not implemented`)
  // compilerAssert(astWriter[ast.key], `Not implemented ast writer '${ast.key}'`)
  // const nodeInfo: NodeInfo = { memoryPointer: undefined }
  // Object.assign(nodeInfo, opts)
  // astWriter[ast.key](writer, ast as any, nodeInfo);
};

const visitAst = (writer: LlvmFunctionWriter, ast: Ast, opts?: Partial<NodeInfo>) => {
  compilerAssert(astWriter[ast.key], `Not implemented ast writer '${ast.key}'`)
  const nodeInfo: NodeInfo = { memoryPointer: undefined }
  Object.assign(nodeInfo, opts)
  return astWriter[ast.key](writer, ast as any, nodeInfo);
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

const toRegister = (writer: LlvmFunctionWriter, v: LlvmResultValue): string => {
  if ('register' in v) return v.register
  const name = `%${generateName(writer.writer, new Binding("", VoidType))}`
  // compilerAssert(!('value' in v), "Not impl")
  format(writer, "  $ = load $, ptr $\n", name, v.type, v.pointer)
  return name
}
const toPointer = (writer: LlvmFunctionWriter, v: LlvmResultValue, type: Type): string => {
  if ('pointer' in v) return v.pointer
  const ptrName = `%${generateName(writer.writer, new Binding("", VoidType))}`
  format(writer, "  $ = alloca $\n", ptrName, type)
  format(writer, "  store $ $, ptr $\n", type, v.register, ptrName)
  return ptrName
}
const getPointerName = (writer: LlvmFunctionWriter, type: Type) => {
  if (type === RawPointerType) return 'ptr'
  return `${getTypeName(writer.writer, type)}*`
}

type NodeInfo = {
  memoryPointer: string | undefined
}
class RawNode        { key = 'RawNode';        constructor(public string: string) {} }
class StoreNode      { key = 'StoreNode';      constructor(public binding: Binding, public expr: Node) {} }
class LoadNode       { key = 'LoadNode';       constructor(public binding: Binding, public expr: Node) {} }
class RegisterNode   { key = 'RegisterNode';   constructor(public binding: Binding) {} }
class PointerNode    { key = 'PointerNode';    constructor(public binding: Binding) {} }
class StatementsNode { key = 'StatementsNode'; constructor(public statements: Node[]) {} }

type Node = RawNode | StoreNode | LoadNode | StatementsNode | RegisterNode

export type LlvmAstWriterTable = {
  [A in Ast as A['key']]: (writer: LlvmFunctionWriter, ast: A, nodeInfo: NodeInfo) => Node;
}

const astWriterOld: unknown = {
  statements: (writer, ast, nodeInfo) => {
    // TODO: Filter voids?
    ast.statements.forEach((expr, i) => {
      writer.writer.outputStrings.push("; Statement\n")
      const n = i === ast.statements.length - 1 ? nodeInfo : {}
      writeExpr(writer, expr, n)
      writer.writer.outputStrings.push("\n")
      // compilerAssert(writer.nameStack.length === 0, "Unexpected name in stack", { nameStack: writer.nameStack })
      const toPop = i !== ast.statements.length - 1 || ast.type === VoidType
      if (toPop) {
        writer.valueStack.pop()
      }
    })
  },
  string: (writer, ast, nodeInfo) => {
    const constantName = `@${generateName(writer.writer, new Binding("constant", VoidType))}`
 
    const escaped = ast.value.replace(/["\\]|[\x00-\x1F\x80-\xFF]/g, (str) => {
      return `\\${str.charCodeAt(0).toString(16)}`
    })
    const length = ast.value.length + 1 // add null terminator
    writer.writer.outputHeaders.push(`${constantName} = private unnamed_addr constant [${length} x i8] c"${escaped}\\00"\n`)
    // const named = !!writer.nameStack.length
    const name = nodeInfo.memoryPointer ?? `%${generateName(writer.writer, new Binding("", VoidType))}`
    format(writer, "  $ = alloca $\n", name, ast.type)
    format(writer, `  store $ { i32 $, ptr $ }, ptr $\n`, ast.type, String(length - 1), constantName, name)
    writer.valueStack.push({ pointer: name, type: ast.type })
  },
  cast: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },
  binding: (writer, ast, nodeInfo) => {
    compilerAssert(ast.binding.type !== VoidType)
    const isArg = !!writer.function.argBindings.find(x => x === ast.binding)

    if (nodeInfo.memoryPointer) {
      format(writer, "  $ = alloca $\n", nodeInfo.memoryPointer, ast.binding.type)
    }
    
    if (isArg) {
      const name = generateName(writer.writer, ast.binding)
      writer.valueStack.push({ register: `%${name}` })
      return
    }

    // const ptrName = nodeInfo.memoryPointer ??
    //   `%${generateName(writer.writer, new Binding(`${ast.binding.name}.value`, VoidType))}`
    const ptrName = `%${generateName(writer.writer, ast.binding)}`
    writer.valueStack.push({ pointer: ptrName, type: ast.type })
    // format(writer, "  $ = load $, ptr $\n", name, ast.binding.type, ptrName)
    // writer.valueStack.push(name)
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented", { ast })
    
    const ptrName = `%${generateName(writer.writer, ast.binding)}`
    // format(writer, "  $ = alloca $\n", ptrName, ast.binding.type)

    // writer.nameStack.push(ptrName)
    writeExpr(writer, ast.value, { memoryPointer: ptrName })
    console.log("After let", writer.valueStack)
    const result = writer.valueStack.pop()!
    if ('register' in result) {
      if (result.register !== ptrName) {
        format(writer, "  store $ $, ptr $\n", ast.binding.type, result.register, ptrName)
      }
    } else if (result.pointer !== ptrName) {
      const structType = ast.binding.type
      const temp = `%${generateName(writer.writer, new Binding("", VoidType))}`
      format(writer, "  $ = load $, ptr $\n", temp, structType, result.pointer)
      format(writer, "  store $ $, ptr $\n", structType, temp, ptrName)
    }

  },
  set: (writer, ast) => {
    const name = `%${generateName(writer.writer, new Binding(`${ast.binding.name}.value`, VoidType))}`
    writeExpr(writer, ast.value)

    const ptrName = `%${generateName(writer.writer, ast.binding)}`
    format(writer, "  store $ $, ptr $\n", ast.binding.type, name, ptrName)
  },
  number: (writer, ast, nodeInfo) => {
    compilerAssert(ast.type === IntType || ast.type === FloatType || ast.type === DoubleType, "Expected number type got $type", { ast, type: ast.type })
    // const ptrName = popNameStackOrGenerateNewName(writer)

    if (nodeInfo.memoryPointer) {
      format(writer, "  $ = alloca $\n", nodeInfo.memoryPointer, ast.type)
    }

    const valueName = `%${generateName(writer.writer, new Binding("", VoidType))}`
    format(writer, `  $ = add $ 0, $ ; literal\n`, valueName, ast.type, String(ast.value))
    if (nodeInfo.memoryPointer) {
      format(writer, `  store $ $, ptr $\n`, ast.type, valueName, nodeInfo.memoryPointer)
      writer.valueStack.push({ pointer: nodeInfo.memoryPointer, type: ast.type })
    } else {
      writer.valueStack.push({ register: valueName })
    }

  },
  bool: (writer, ast) => {
    const name = popNameStackOrGenerateNewName(writer)
    format(writer, `  $ = add $ 0, $ ; literal\n`, name, ast.type, ast.value ? "1" : "0")
    writer.valueStack.push({ register: name })
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
    if (outName) thenVal = toRegister(writer, writer.valueStack.pop()!)
    format(writer, `  br label %$\n\n`, endLabel)

    if (ast.falseBody) {
      format(writer, `$:\n`, elseLabel)
      writer.currentBlockLabel = elseLabel
      writeExpr(writer, toStatements(ast.falseBody))
      elseFinalLabel = writer.currentBlockLabel // Nested control flow have have changed current block
      if (outName) elseVal = toRegister(writer, writer.valueStack.pop()!)
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
    const bVal = toRegister(writer, writer.valueStack.pop()!)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultFalse)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultLabel)
    format(writer, `  $ = phi $ [ false, %$ ], [ $, %$ ]\n`, outName, ast.type, resultFalse, bVal, secondOperandFinalName)
    writer.currentBlockLabel = resultLabel
    writer.valueStack.push({ register: outName })
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
    const bVal = toRegister(writer, writer.valueStack.pop()!)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultTrue)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultLabel)
    format(writer, `  $ = phi $ [ true, %$ ], [ $, %$ ]\n`, outName, ast.type, resultTrue, bVal, secondOperandFinalName)
    writer.currentBlockLabel = resultLabel
    writer.valueStack.push({ register: outName })
  },
  while: (writer, ast) => {
    const loopCondition = generateName(writer.writer, new Binding(`while_condition`, VoidType))
    const loopBody = generateName(writer.writer, new Binding(`while_body`, VoidType))
    const loopEnd = generateName(writer.writer, new Binding(`while_end`, VoidType))

    format(writer, `  br label %$\n`, loopCondition)
    format(writer, `$:\n`, loopCondition)
    writer.currentBlockLabel = loopCondition
    writeExpr(writer, ast.condition)
    const aVal = toRegister(writer, writer.valueStack.pop()!)
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
      const named = !!writer.nameStack.length
      const outName = named && popNameStackOrGenerateNewName(writer)
      const name = `%${generateName(writer.writer, new Binding("", VoidType))}`
      const formatPtrName = ast.args[0].type === IntType ? `@format_string_int` :
        ast.args[0].type === FloatType ? `@format_string_float` :
        ast.args[0].type === RawPointerType ? `@format_string_ptr` :
        ast.args[0].type === StringType ? `@format_string_string` : undefined
      compilerAssert(formatPtrName, "Not implemented for this type", { type: ast.args[0].type })
      writeExpr(writer, ast.args[0])
      const result = toRegister(writer, writer.valueStack.pop()!)

      if (ast.args[0].type === StringType) {
        const fields = ast.args[0].type.typeInfo.fields

        const loadField = (basePointer: string, structType: Type, field: TypeField) => {
          const fieldType = field.fieldType
          const loadFieldPtr = `%${generateName(writer.writer, new Binding("", VoidType))}`
          const loadFieldVal = `%${generateName(writer.writer, new Binding("", VoidType))}`
          format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, structType, getPointerName(writer, structType), basePointer, String(field.index))
          format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
          return `${getTypeName(writer.writer, field.fieldType)} ${loadFieldVal}`
        }
        const basePointer = `%${generateName(writer.writer, new Binding("", VoidType))}`
        format(writer, "  $ = alloca $\n", basePointer, StringType)
        format(writer, "  store $ $, ptr $\n", StringType, result, basePointer)
        const lengthRegister = loadField(basePointer, StringType, fields.find(x => x.name === 'length')!)
        const strPtrRegister = loadField(basePointer, StringType, fields.find(x => x.name === 'data')!)
        format(writer, "  $ = call i32 (i8*, ...) @printf(ptr $, $, $)\n", name, formatPtrName, lengthRegister, strPtrRegister)
      } else {
        format(writer, "  $ = call i32 (i8*, ...) @printf(ptr $, $ $)\n", name, formatPtrName, ast.args[0].type, result)
      }
      if (outName) {
        format(writer, "  ; write to $\n", outName)
      }
      return
    }

    compilerAssert(false, "Not implemented")
  },
  list: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },

  usercall: (writer, ast, nodeInfo) => {
    const named = !!writer.nameStack.length
    // const ptrName = ast.type !== VoidType && named ? popNameStackOrGenerateNewName(writer) : undefined
    const name = ast.type !== VoidType && `%${generateName(writer.writer, new Binding("", VoidType))}`

    if (nodeInfo.memoryPointer) {
      format(writer, "  $ = alloca $\n", nodeInfo.memoryPointer, ast.type)
    }

    const argValues = ast.args.map(arg => {
      writeExpr(writer, arg); return toRegister(writer, writer.valueStack.pop()!)
    })
    
    if (name) { format(writer, `  $ = `, name) }
    else { format(writer, `  `) }
    
    format(writer, `call $ @$(`, ast.type, ast.binding)
    ast.args.forEach((arg, i) => {
      if (i !== 0) format(writer, ", ")
      format(writer, `$ $`, arg.type, argValues[i]!)
    })
    format(writer, `)\n`)
    if (name) writer.valueStack.push({ register: name })
  },
  operator: (writer, ast) => {
    const [a, b] = ast.args
    const name = popNameStackOrGenerateNewName(writer)
    compilerAssert(a.type === b.type, "Expected types to be equal", { a, b })
    const op = operatorMap[ast.operator]
    compilerAssert(op, "Expected op", { ast })
    format(writer, `  $ = $ $ $, $\n`, name, op, a.type, a, b)
    writer.valueStack.push({ register: name })
  },
  not: (writer, ast) => {
    const expr = ast.expr
    compilerAssert(expr.type === BoolType, "Expected bool")
    const name = popNameStackOrGenerateNewName(writer)
    format(writer, `  $ = xor $ $, 1\n`, name, expr.type, expr)
    writer.valueStack.push({ register: name })
  },
  defaultcons: (writer, ast) => {
    compilerAssert(false, "Not implemented 'defaultcons'")
  },
  constructor: (writer, ast, nodeInfo) => {

    // format(writer, "  ; string literal: $\n", ast.value)
    // format(writer, "  $ = getelementptr [$ x i8], [$ x i8]* $, i64 0, i64 0\n", strPtr, String(length), String(length), constantName)
    // format(writer, "  $ = getelementptr $, $* $, i32 0, i32 0\n", lengthFieldName, ast.type, ast.type, name)
    // format(writer, "  store i32 $, i32* $\n", String(length - 1), lengthFieldName)
    // format(writer, "  $ = getelementptr $, $* $, i32 0, i32 1\n", ptrFieldName, ast.type, ast.type, name)
    // format(writer, "  store ptr $, ptr $\n", strPtr, ptrFieldName)

    // const named = !!writer.nameStack.length

    console.log("constructor", nodeInfo)

    const structPtr = nodeInfo.memoryPointer ?? `%${generateName(writer.writer, new Binding("", VoidType))}`
    format(writer, "  $ = alloca $\n", structPtr, ast.type)

    // if (!named) format(writer, "  $ = alloca $\n", structPtr, ast.type)

    ast.args.forEach((arg, index) => {
      writeExpr(writer, arg)
      const result = writer.valueStack.pop()!

      const fieldPtr = `%${generateName(writer.writer, new Binding(``, VoidType))}`
      const field = ast.type.typeInfo.fields[index]

      format(writer, "  $ = getelementptr $, ptr $, i32 0, i32 $\n", fieldPtr, ast.type, structPtr, String(index))
      const reg = toRegister(writer, result)
      // if ('register' in result) {
        format(writer, "  store $ $, $ $\n", field.fieldType, reg, getPointerName(writer, field.fieldType), fieldPtr)
      // } else {
      //   format(writer, "  ; store $ $, $ $\n", field.fieldType, result.pointer, getPointerName(writer, field.fieldType), fieldPtr)
      //   // compilerAssert(false, "Not impl")
      // }
    })

    writer.valueStack.push({ pointer: structPtr, type: ast.type })
    // format(writer, "  $ = alloca $\n", name, ast.type)
    

    
    // compilerAssert(false, "Not implemented 'constructor'")
  },
  valuefield: (writer, ast) => {

    // const storeFieldVal = `%${generateName(writer.writer, new Binding("", VoidType))}`
    // format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, structType, getPointerName(writer, structType), result.pointer, String(field.index))
    // format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
    const loadField = (base: LlvmResultValue, field: TypeField) => {
      const fieldType = field.fieldType
      
      const loadFieldPtr = `%${generateName(writer.writer, new Binding("", VoidType))}`
      const loadFieldVal = `%${generateName(writer.writer, new Binding("", VoidType))}`

      if ('pointer' in base) {
        format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, field.sourceType, getPointerName(writer, field.sourceType), base.pointer, String(field.index))
        format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
      } else {
        format(writer, "  $ = extractvalue $ $, $\n", loadFieldVal, field.sourceType, base.register, String(field.index))
      }
      return { register: loadFieldVal }
    }
    writeExpr(writer, ast.left)
    const leftResult = writer.valueStack.pop()!
    // console.log(result)
    const reg = ast.fieldPath.reduce((reg, field) => {
      return loadField(leftResult, ast.fieldPath[0])
    }, leftResult)

    writer.valueStack.push(reg)

    // compilerAssert(false, "Not implemented 'valuefield'")
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
const astWriter: LlvmAstWriterTable = {
  statements: (writer, ast, nodeInfo) => {
    // TODO: Filter voids?
    const res: Node[] = []
    ast.statements.forEach((expr, i) => {
      // writer.writer.outputStrings.push("; Statement\n")
      const n = i === ast.statements.length - 1 ? nodeInfo : {}
      res.push(visitAst(writer, expr, n))
      // writer.writer.outputStrings.push("\n")
      // compilerAssert(writer.nameStack.length === 0, "Unexpected name in stack", { nameStack: writer.nameStack })
    })
    return new StatementsNode(res)
  },
  string: (writer, ast, nodeInfo) => {
    const constantName = `@${generateName(writer.writer, new Binding("constant", VoidType))}`
 
    const escaped = ast.value.replace(/["\\]|[\x00-\x1F\x80-\xFF]/g, (str) => {
      return `\\${str.charCodeAt(0).toString(16)}`
    })
    const length = ast.value.length + 1 // add null terminator
    // writer.writer.outputHeaders.push(`${constantName} = private unnamed_addr constant [${length} x i8] c"${escaped}\\00"\n`)
    // // const named = !!writer.nameStack.length
    // const name = nodeInfo.memoryPointer ?? `%${generateName(writer.writer, new Binding("", VoidType))}`
    // format(writer, "  $ = alloca $\n", name, ast.type)
    // format(writer, `  store $ { i32 $, ptr $ }, ptr $\n`, ast.type, String(length - 1), constantName, name)
    // writer.valueStack.push({ pointer: name, type: ast.type })
    return new StoreNode(null!, new RawNode(`; string literal`))
  },
  binding: (writer, ast, nodeInfo) => {
    compilerAssert(ast.binding.type !== VoidType)
    const isArg = !!writer.function.argBindings.find(x => x === ast.binding)

    // if (nodeInfo.memoryPointer) {
    //   format(writer, "  $ = alloca $\n", nodeInfo.memoryPointer, ast.binding.type)
    // }
    
    if (isArg) {
      return new RegisterNode(ast.binding)
    }
    return new PointerNode(ast.binding)

    // const ptrName = nodeInfo.memoryPointer ??
    //   `%${generateName(writer.writer, new Binding(`${ast.binding.name}.value`, VoidType))}`
    const ptrName = `%${generateName(writer.writer, ast.binding)}`
    writer.valueStack.push({ pointer: ptrName, type: ast.type })
    // format(writer, "  $ = load $, ptr $\n", name, ast.binding.type, ptrName)
    // writer.valueStack.push(name)
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented", { ast })
    
    // const ptrName = `%${generateName(writer.writer, ast.binding)}`
    // format(writer, "  $ = alloca $\n", ptrName, ast.binding.type)

    // writer.nameStack.push(ptrName)
    const value = visitAst(writer, ast.value)
    return new StoreNode(ast.binding, value)

    // console.log("After let", writer.valueStack)
    // const result = writer.valueStack.pop()!
    // if ('register' in result) {
    //   if (result.register !== ptrName) {
    //     format(writer, "  store $ $, ptr $\n", ast.binding.type, result.register, ptrName)
    //   }
    // } else if (result.pointer !== ptrName) {
    //   const structType = ast.binding.type
    //   const temp = `%${generateName(writer.writer, new Binding("", VoidType))}`
    //   format(writer, "  $ = load $, ptr $\n", temp, structType, result.pointer)
    //   format(writer, "  store $ $, ptr $\n", structType, temp, ptrName)
    // }

  },
  set: (writer, ast) => {
    compilerAssert(false, "Not impl")
    // const name = `%${generateName(writer.writer, new Binding(`${ast.binding.name}.value`, VoidType))}`
    // writeExpr(writer, ast.value)

    // const ptrName = `%${generateName(writer.writer, ast.binding)}`
    // format(writer, "  store $ $, ptr $\n", ast.binding.type, name, ptrName)
  },
  number: (writer, ast, nodeInfo) => {
    compilerAssert(ast.type === IntType || ast.type === FloatType || ast.type === DoubleType, "Expected number type got $type", { ast, type: ast.type })
    // const ptrName = popNameStackOrGenerateNewName(writer)

    return new RawNode(`add $ 0, $ ; number literal`)
    // if (nodeInfo.memoryPointer) {
    //   format(writer, "  $ = alloca $\n", nodeInfo.memoryPointer, ast.type)
    // }

    // const valueName = `%${generateName(writer.writer, new Binding("", VoidType))}`
    // format(writer, `  $ = add $ 0, $ ; literal\n`, valueName, ast.type, String(ast.value))
    // if (nodeInfo.memoryPointer) {
    //   format(writer, `  store $ $, ptr $\n`, ast.type, valueName, nodeInfo.memoryPointer)
    //   writer.valueStack.push({ pointer: nodeInfo.memoryPointer, type: ast.type })
    // } else {
    //   writer.valueStack.push({ register: valueName })
    // }

  },
  bool: (writer, ast) => {
    // const name = popNameStackOrGenerateNewName(writer)
    // format(writer, `  $ = add $ 0, $ ; literal\n`, name, ast.type, ast.value ? "1" : "0")
    // writer.valueStack.push({ register: name })
    return new RawNode(`add $ 0, $ ; bool literal`)

  },
  if: (writer, ast) => {
    compilerAssert(false, "Not impl")
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
    if (outName) thenVal = toRegister(writer, writer.valueStack.pop()!)
    format(writer, `  br label %$\n\n`, endLabel)

    if (ast.falseBody) {
      format(writer, `$:\n`, elseLabel)
      writer.currentBlockLabel = elseLabel
      writeExpr(writer, toStatements(ast.falseBody))
      elseFinalLabel = writer.currentBlockLabel // Nested control flow have have changed current block
      if (outName) elseVal = toRegister(writer, writer.valueStack.pop()!)
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
    compilerAssert(false, "Not impl")
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
    const bVal = toRegister(writer, writer.valueStack.pop()!)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultFalse)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultLabel)
    format(writer, `  $ = phi $ [ false, %$ ], [ $, %$ ]\n`, outName, ast.type, resultFalse, bVal, secondOperandFinalName)
    writer.currentBlockLabel = resultLabel
    writer.valueStack.push({ register: outName })
  },
  or: (writer, ast) => {
    compilerAssert(false, "Not impl")
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
    const bVal = toRegister(writer, writer.valueStack.pop()!)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultTrue)
    format(writer, `  br label %$\n`, resultLabel)
    format(writer, `$:\n`, resultLabel)
    format(writer, `  $ = phi $ [ true, %$ ], [ $, %$ ]\n`, outName, ast.type, resultTrue, bVal, secondOperandFinalName)
    writer.currentBlockLabel = resultLabel
    writer.valueStack.push({ register: outName })
  },
  while: (writer, ast) => {
    compilerAssert(false, "Not impl")
    const loopCondition = generateName(writer.writer, new Binding(`while_condition`, VoidType))
    const loopBody = generateName(writer.writer, new Binding(`while_body`, VoidType))
    const loopEnd = generateName(writer.writer, new Binding(`while_end`, VoidType))

    format(writer, `  br label %$\n`, loopCondition)
    format(writer, `$:\n`, loopCondition)
    writer.currentBlockLabel = loopCondition
    writeExpr(writer, ast.condition)
    const aVal = toRegister(writer, writer.valueStack.pop()!)
    format(writer, `  br i1 $, label %$, label %$\n\n`, aVal, loopBody, loopEnd)

    format(writer, `$:\n`, loopBody)
    writer.currentBlockLabel = loopBody
    writeExpr(writer, ast.body)
    format(writer, `  br label %$\n\n`, loopCondition)

    format(writer, `$:\n`, loopEnd)
    writer.currentBlockLabel = loopEnd
  },

  block: (writer, ast) => {
    compilerAssert(false, "Not impl")
    writer.blocks.push({ binding: ast.binding }) // not strictly necessary?
    writeExpr(writer, ast.body)
    writer.blocks.pop()!
    const blockEndLabel = generateName(writer.writer, ast.binding)
    format(writer, `  br label %$\n\n`, blockEndLabel)
    format(writer, `$:\n`, blockEndLabel)
    writer.currentBlockLabel = blockEndLabel
  },
  break: (writer, ast) => {
    compilerAssert(false, "Not impl")
    const block = writer.blocks.findLast(x => x.binding === ast.binding)
    compilerAssert(block, "Programmer error. Expected block") // Programmer error
    format(writer, `  br label %$\n`, ast.binding)
  },
  call: (writer, ast) => {
    if (ast.func.name === "print") {
      return new RawNode("call print")
      // compilerAssert(ast.args.length === 1, "Print not implemented yet", { ast });
      // const named = !!writer.nameStack.length
      // const outName = named && popNameStackOrGenerateNewName(writer)
      // const name = `%${generateName(writer.writer, new Binding("", VoidType))}`
      // const formatPtrName = ast.args[0].type === IntType ? `@format_string_int` :
      //   ast.args[0].type === FloatType ? `@format_string_float` :
      //   ast.args[0].type === RawPointerType ? `@format_string_ptr` :
      //   ast.args[0].type === StringType ? `@format_string_string` : undefined
      // compilerAssert(formatPtrName, "Not implemented for this type", { type: ast.args[0].type })
      // writeExpr(writer, ast.args[0])
      // const result = toRegister(writer, writer.valueStack.pop()!)

      // if (ast.args[0].type === StringType) {
      //   const fields = ast.args[0].type.typeInfo.fields

      //   const loadField = (basePointer: string, structType: Type, field: TypeField) => {
      //     const fieldType = field.fieldType
      //     const loadFieldPtr = `%${generateName(writer.writer, new Binding("", VoidType))}`
      //     const loadFieldVal = `%${generateName(writer.writer, new Binding("", VoidType))}`
      //     format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, structType, getPointerName(writer, structType), basePointer, String(field.index))
      //     format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
      //     return `${getTypeName(writer.writer, field.fieldType)} ${loadFieldVal}`
      //   }
      //   const basePointer = `%${generateName(writer.writer, new Binding("", VoidType))}`
      //   format(writer, "  $ = alloca $\n", basePointer, StringType)
      //   format(writer, "  store $ $, ptr $\n", StringType, result, basePointer)
      //   const lengthRegister = loadField(basePointer, StringType, fields.find(x => x.name === 'length')!)
      //   const strPtrRegister = loadField(basePointer, StringType, fields.find(x => x.name === 'data')!)
      //   format(writer, "  $ = call i32 (i8*, ...) @printf(ptr $, $, $)\n", name, formatPtrName, lengthRegister, strPtrRegister)
      // } else {
      //   format(writer, "  $ = call i32 (i8*, ...) @printf(ptr $, $ $)\n", name, formatPtrName, ast.args[0].type, result)
      // }
      // if (outName) {
      //   format(writer, "  ; write to $\n", outName)
      // }
      // return
    }

    compilerAssert(false, "Not implemented")
  },
  list: (writer, ast) => {
    compilerAssert(false, "Not implemented")
  },

  usercall: (writer, ast, nodeInfo) => {
    return new RawNode("user call")
    // compilerAssert(false, "Not implemented")
    const named = !!writer.nameStack.length
    // const ptrName = ast.type !== VoidType && named ? popNameStackOrGenerateNewName(writer) : undefined
    const name = ast.type !== VoidType && `%${generateName(writer.writer, new Binding("", VoidType))}`

    if (nodeInfo.memoryPointer) {
      format(writer, "  $ = alloca $\n", nodeInfo.memoryPointer, ast.type)
    }

    const argValues = ast.args.map(arg => {
      writeExpr(writer, arg); return toRegister(writer, writer.valueStack.pop()!)
    })
    
    if (name) { format(writer, `  $ = `, name) }
    else { format(writer, `  `) }
    
    format(writer, `call $ @$(`, ast.type, ast.binding)
    ast.args.forEach((arg, i) => {
      if (i !== 0) format(writer, ", ")
      format(writer, `$ $`, arg.type, argValues[i]!)
    })
    format(writer, `)\n`)
    if (name) writer.valueStack.push({ register: name })
  },
  operator: (writer, ast) => {
    compilerAssert(false, "Not implemented")
    const [a, b] = ast.args
    const name = popNameStackOrGenerateNewName(writer)
    compilerAssert(a.type === b.type, "Expected types to be equal", { a, b })
    const op = operatorMap[ast.operator]
    compilerAssert(op, "Expected op", { ast })
    format(writer, `  $ = $ $ $, $\n`, name, op, a.type, a, b)
    writer.valueStack.push({ register: name })
  },
  not: (writer, ast) => {
    compilerAssert(false, "Not implemented")
    const expr = ast.expr
    compilerAssert(expr.type === BoolType, "Expected bool")
    const name = popNameStackOrGenerateNewName(writer)
    format(writer, `  $ = xor $ $, 1\n`, name, expr.type, expr)
    writer.valueStack.push({ register: name })
  },
  defaultcons: (writer, ast) => {
    compilerAssert(false, "Not implemented 'defaultcons'")
  },
  constructor: (writer, ast, nodeInfo) => {
    compilerAssert(false, "Not implemented")

    // format(writer, "  ; string literal: $\n", ast.value)
    // format(writer, "  $ = getelementptr [$ x i8], [$ x i8]* $, i64 0, i64 0\n", strPtr, String(length), String(length), constantName)
    // format(writer, "  $ = getelementptr $, $* $, i32 0, i32 0\n", lengthFieldName, ast.type, ast.type, name)
    // format(writer, "  store i32 $, i32* $\n", String(length - 1), lengthFieldName)
    // format(writer, "  $ = getelementptr $, $* $, i32 0, i32 1\n", ptrFieldName, ast.type, ast.type, name)
    // format(writer, "  store ptr $, ptr $\n", strPtr, ptrFieldName)

    // const named = !!writer.nameStack.length

    console.log("constructor", nodeInfo)

    const structPtr = nodeInfo.memoryPointer ?? `%${generateName(writer.writer, new Binding("", VoidType))}`
    format(writer, "  $ = alloca $\n", structPtr, ast.type)

    // if (!named) format(writer, "  $ = alloca $\n", structPtr, ast.type)

    ast.args.forEach((arg, index) => {
      writeExpr(writer, arg)
      const result = writer.valueStack.pop()!

      const fieldPtr = `%${generateName(writer.writer, new Binding(``, VoidType))}`
      const field = ast.type.typeInfo.fields[index]

      format(writer, "  $ = getelementptr $, ptr $, i32 0, i32 $\n", fieldPtr, ast.type, structPtr, String(index))
      const reg = toRegister(writer, result)
      // if ('register' in result) {
        format(writer, "  store $ $, $ $\n", field.fieldType, reg, getPointerName(writer, field.fieldType), fieldPtr)
      // } else {
      //   format(writer, "  ; store $ $, $ $\n", field.fieldType, result.pointer, getPointerName(writer, field.fieldType), fieldPtr)
      //   // compilerAssert(false, "Not impl")
      // }
    })

    writer.valueStack.push({ pointer: structPtr, type: ast.type })
    // format(writer, "  $ = alloca $\n", name, ast.type)
    

    
    // compilerAssert(false, "Not implemented 'constructor'")
  },
  valuefield: (writer, ast) => {
    compilerAssert(false, "Not implemented")

    // const storeFieldVal = `%${generateName(writer.writer, new Binding("", VoidType))}`
    // format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, structType, getPointerName(writer, structType), result.pointer, String(field.index))
    // format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
    const loadField = (base: LlvmResultValue, field: TypeField) => {
      const fieldType = field.fieldType
      
      const loadFieldPtr = `%${generateName(writer.writer, new Binding("", VoidType))}`
      const loadFieldVal = `%${generateName(writer.writer, new Binding("", VoidType))}`

      if ('pointer' in base) {
        format(writer, "  $ = getelementptr $, $ $, i32 0, i32 $\n", loadFieldPtr, field.sourceType, getPointerName(writer, field.sourceType), base.pointer, String(field.index))
        format(writer, "  $ = load $, $ $\n", loadFieldVal, fieldType, getPointerName(writer, fieldType), loadFieldPtr)
      } else {
        format(writer, "  $ = extractvalue $ $, $\n", loadFieldVal, field.sourceType, base.register, String(field.index))
      }
      return { register: loadFieldVal }
    }
    writeExpr(writer, ast.left)
    const leftResult = writer.valueStack.pop()!
    // console.log(result)
    const reg = ast.fieldPath.reduce((reg, field) => {
      return loadField(leftResult, ast.fieldPath[0])
    }, leftResult)

    writer.valueStack.push(reg)

    // compilerAssert(false, "Not implemented 'valuefield'")
  },
  cast: (writer, ast) => {
    compilerAssert(false, "Not implemented 'cast'")
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
    compilerAssert(false, "Not implemented")
  }
};

const format = (writer: LlvmFunctionWriter, format: string, ...args: (string | Type | Ast | Binding)[]) => {
  let i = 0
  const s = format.replace(/\$/g, (x) => {
    const v = args[i++]
    
    if (typeof v === 'string') return v
    if (isType(v)) return getTypeName(writer.writer, v)
    if (v instanceof Binding) { return generateName(writer.writer, v) }
    if (isAst(v)) {
      compilerAssert(false, "not impl")
      writeExpr(writer, v)
      compilerAssert(writer.valueStack.length, "Expected value")
      const result = writer.valueStack.pop()!
      return toRegister(writer, result)
      // if ('register' in result) return result.register
      // return result.pointer
    }
    return ''
  })
  writer.writer.outputStrings.push(s)
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
    outputHeaders: [],
    outputStrings: [],
    outputWriter,
  }

  bytecodeWriter.globalNames.set(VoidType, "void")
  bytecodeWriter.globalNames.set(IntType, "i32")
  bytecodeWriter.globalNames.set(BoolType, "i1")
  bytecodeWriter.globalNames.set(FloatType, "f32")
  bytecodeWriter.globalNames.set(RawPointerType, "ptr")

  bytecodeWriter.outputHeaders.push("declare i32 @printf(i8*, ...)\n\n")
  bytecodeWriter.outputHeaders.push(`@format_string_int = private unnamed_addr constant [4 x i8] c"%i\\0A\\00", align 1\n`)
  bytecodeWriter.outputHeaders.push(`@format_string_float = private unnamed_addr constant [4 x i8] c"%f\\0A\\00", align 1\n`)
  bytecodeWriter.outputHeaders.push(`@format_string_ptr = private unnamed_addr constant [4 x i8] c"%p\\0A\\00", align 1\n`)
  bytecodeWriter.outputHeaders.push(`@format_string_string = private unnamed_addr constant [6 x i8] c"%.*s\\0A\\00", align 1\n`)
  bytecodeWriter.outputHeaders.push(`\n`)

  Array.from(globalCompilerState.compiledFunctions.values()).map(func => {
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

const generateName = (writer: LlvmWriter, binding: Binding) => {
  if (writer.globalNames.get(binding)) {
    return writer.globalNames.get(binding)!
  }
  let name = binding.name.replace(/[^a-zA-Z0-9_\.]/g, ' ').trim().replace(/ +/g, '_')

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
  const name = `%struct.${generateName(writer, new Binding(obj.shortName, VoidType))}`
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
    nameStack: [],
    valueStack: [],
    currentBlockLabel: '<no name>',
    blocks: [],
    constantsByType: new Map(),
  }

  const isMain = bytecodeWriter.globalCompilerState.entryFunction === func
  const name = isMain ? "main" : generateName(bytecodeWriter, func.binding)
  
  format(funcWriter, `define $ @$(`, func.returnType, name)
  func.argBindings.forEach((binding, i) => {
    if (i !== 0) format(funcWriter, ", ")
    format(funcWriter, `$ %$`, binding.type, binding)
  })
  format(funcWriter, `) {\n`, func.returnType, name)
  const result = visitAst(funcWriter, func.body)
  console.log(result)
  // if (func.returnType !== VoidType) {
  //   compilerAssert(funcWriter.valueStack.length === 1, "Expected 1 value left", { valueStack: funcWriter.valueStack })
  //   const v = toRegister(funcWriter, funcWriter.valueStack.pop()!)
  //   format(funcWriter, `  ret $ $\n`, func.returnType, v)
  // } else {
  //   format(funcWriter, `  ret void\n`)
  // }
  bytecodeWriter.outputStrings.push(`}\n\n`)

  return funcWriter
};
