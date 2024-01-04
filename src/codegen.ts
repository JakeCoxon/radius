import { Ast, AstWriterTable, Binding, BindingAst, BoolType, CodegenFunctionWriter, CodegenWriter, CompiledFunction, ConcreteClassType, DoubleType, ExternalTypeConstructor, FileWriter, FloatType, GlobalCompilerState, IntType, ListTypeConstructor, ParameterizedType, PrimitiveType, RawPointerType, Type, TypeField, VoidType, compilerAssert, textColors } from "./defs";

const OpCodes = {
  Nil: 0,
  True: 1,
  False: 2,
  And: 3,
  Or: 4,
  Pop: 5,
  PopResult: 6,
  Print: 7,
  StringFormat: 8,
  Jump: 9,
  JumpIfFalse: 10,
  JumpIfFalsePop: 11,
  Loop: 12,
  Call: 13,
  Return: 14,
  GetField: 15,
  SubscriptList: 16,
  List: 17,
  Alloc: 18,
  ConstantS: 19,
  GetLocalS: 20,
  SetLocalS: 21,
  F32_TO_I32: 22,
  I32_TO_F32: 23,
  CheckStack: 24,
  ConstantV: 25,
  ConstantF32: 26,
  ConstantF64: 27,
  ConstantI32: 28,
  ConstantI64: 29,
  GetLocalV: 30,
  GetLocalF32: 31,
  GetLocalF64: 32,
  GetLocalI32: 33,
  GetLocalI64: 34,
  SetLocalV: 35,
  SetLocalF32: 36,
  SetLocalF64: 37,
  SetLocalI32: 38,
  SetLocalI64: 39,
  EqualV: 40,
  EqualF32: 41,
  EqualF64: 42,
  EqualI32: 43,
  EqualI64: 44,
  GreaterV: 45,
  GreaterF32: 46,
  GreaterF64: 47,
  GreaterI32: 48,
  GreaterI64: 49,
  LessV: 50,
  LessF32: 51,
  LessF64: 52,
  LessI32: 53,
  LessI64: 54,
  AddV: 55,
  AddF32: 56,
  AddF64: 57,
  AddI32: 58,
  AddI64: 59,
  SubtractV: 60,
  SubtractF32: 61,
  SubtractF64: 62,
  SubtractI32: 63,
  SubtractI64: 64,
  MultiplyV: 65,
  MultiplyF32: 66,
  MultiplyF64: 67,
  MultiplyI32: 68,
  MultiplyI64: 69,
  DivideV: 70,
  DivideF32: 71,
  DivideF64: 72,
  DivideI32: 73,
  DivideI64: 74,
  NotV: 75,
  NotF32: 76,
  NotF64: 77,
  NotI32: 78,
  NotI64: 79,
  NegateV: 80,
  NegateF32: 81,
  NegateF64: 82,
  NegateI32: 83,
  NegateI64: 84,
  ToStringV: 85,
  ToStringF32: 86,
  ToStringF64: 87,
  ToStringI32: 88,
  ToStringI64: 89,
};

const POINTER_SIZE = 2; // 64 bit

const operatorMap = {
  "+": "Add",
  "-": "Subtract",
  "*": "Multiply",
  "/": "Divide",
  "==": "Equal",
  ">": "Greater",
  "<": "Less",
};


const writeBytes = (writer: CodegenFunctionWriter, ...values: number[]) => {
  values.forEach((x) => compilerAssert(x < 2 ** 8, `Expected ${x} < 256`));
  const name = Object.entries(OpCodes).find(x => x[1] === values[0])?.[0]
  const indent = "  ".repeat(Math.max(writer.nextLocalSlot, 0))
  console.log(textColors.green(String(writer.bytecode.length).padEnd(2, ' ')) + " |" + indent + name, ...values.slice(1))
  writer.bytecode.push(...values);
};
const writeJump = (writer: CodegenFunctionWriter, type: number) => {
  writeBytes(writer, type, 0, 0);
  const jump = writer.bytecode.length;
  return () => {
    writeLittleEndian16At(writer.bytecode, jump - 2, writer.bytecode.length - jump); // prettier-ignore
  }
};
function writeLittleEndian16At(arr: number[], offset: number, number: number) {
  compilerAssert(number < 2 ** 16);
  arr[offset] = number & 0xff; // Write the least significant byte
  arr[offset + 1] = (number >> 8) & 0xff; // Write the most significant byte
}
function writeLittleEndian32At(arr: number[], offset: number, number: number) {
  arr[offset] = number & 0xff; // Least significant byte
  arr[offset + 1] = (number >> 8) & 0xff;
  arr[offset + 2] = (number >> 16) & 0xff;
  arr[offset + 3] = (number >> 24) & 0xff; // Most significant byte
}
const arrayBuffer = new Uint32Array(2);
function writeDoubleLittleEndian(arr: number[], offset: number, number: number) {
  let dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength); // prettier-ignore
  dataView.setFloat64(0, number, true);
  arr[offset + 0] = arrayBuffer[0];
  arr[offset + 1] = arrayBuffer[1];
  // console.log(dataView, arrayBuffer);
}
function writeUint32LittleEndian(arr: number[], offset: number, number: number) {
  let dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength); // prettier-ignore
  dataView.setUint32(0, number, true);
  arr[offset] = arrayBuffer[0];
  // console.log(dataView, arrayBuffer);
}
const writeTypeAt = (arr: number[], offset: number, type: Type, value: number) => {
  compilerAssert(type === IntType || type === BoolType);
  writeUint32LittleEndian(arr, offset, value);
};
const writeOperator = (writer: CodegenFunctionWriter, op: string, type: Type) => {
  if (op === ">=") {
    writeOperator(writer, "<", type);
    writeBytes(writer, OpCodes.NotI32);
    return;
  } else if (op === "<=") {
    writeOperator(writer, ">", type);
    writeBytes(writer, OpCodes.NotI32);
    return;
  }
  let s: string = null!
  if (type === IntType || type === BoolType) s = 'I32'
  else if (type === FloatType) s = 'F32' 
  else if (type === DoubleType) s = 'F64'
  else compilerAssert(false, "Unsupported type $type", { type })
  compilerAssert((operatorMap as any)[op] !== undefined);
  const bytecode: number | undefined = (OpCodes as any)[`${(operatorMap as any)[op]}${s}`];
  compilerAssert(bytecode !== undefined, "No op found", { op, s });
  writeBytes(writer, bytecode);
};
const slotSize = (writer: CodegenFunctionWriter, type: Type): number => {
  if (type === VoidType) return 0
  if (type === IntType) return 1
  if (type === BoolType) return 1
  if (type === FloatType) return 1
  if (type === RawPointerType) return POINTER_SIZE

  if (writer.writer.typeSizes.get(type)) return writer.writer.typeSizes.get(type)!;
  const s = calculateTypeSize(writer, type);
  writer.writer.typeSizes.set(type, s)
  return s
}
const calculateTypeSize = (writer: CodegenFunctionWriter, type: ParameterizedType | ConcreteClassType | PrimitiveType) => {
  if (type.typeInfo.isReferenceType) return POINTER_SIZE;
  return type.typeInfo.fields.reduce((acc, x) => acc + slotSize(writer, x.fieldType), 0)
}
const getSlotOffset = (writer: CodegenFunctionWriter, field: TypeField) => {
  const fields = field.sourceType.typeInfo.fields
  let slotIndex = 0
  for (const f of fields) {
    if (f === field) return slotIndex
    slotIndex += slotSize(writer, f.fieldType)
  }
  compilerAssert(false, "Unreachable")
}
const writeExpr = (writer: CodegenFunctionWriter, ast: Ast) => {
  compilerAssert(astWriter[ast.key], `Not implemented ast writer '${ast.key}'`)
  astWriter[ast.key](writer, ast as any);
};

const emitConstant = (writer: CodegenFunctionWriter, type: Type, value: number) => {
  let index = writer.constants.get(value);
  if (index === undefined) {
    index = writer.nextConstantSlot;
    writer.nextConstantSlot += slotSize(writer, type);
    writer.constants.set(value, index);
    writeTypeAt(writer.constantSlots, writer.constantSlots.length, type, value);
  }
  const opcode = type === IntType || type === BoolType ? OpCodes.ConstantI32 : null
  compilerAssert(opcode, "Not implemented")
  writeBytes(writer, opcode, index);
}

const getFieldOp  = (opcode: number) => ({ opcode, write: (writer: CodegenFunctionWriter, slot: number, size: number) => writeBytes(writer, opcode, slot) })
const getFieldOpS = (opcode: number) => ({ opcode, write: (writer: CodegenFunctionWriter, slot: number, size: number) => writeBytes(writer, opcode, slot, size) })
const GetLocalByType = { I32: getFieldOp(OpCodes.GetLocalI32), I64: getFieldOp(OpCodes.GetLocalI64), F32: getFieldOp(OpCodes.GetLocalF32), F64: getFieldOp(OpCodes.GetLocalF64), S: getFieldOpS(OpCodes.GetLocalS) }
const SetLocalByType = { I32: getFieldOp(OpCodes.SetLocalI32), I64: getFieldOp(OpCodes.SetLocalI64), F32: getFieldOp(OpCodes.SetLocalF32), F64: getFieldOp(OpCodes.SetLocalF64), S: getFieldOpS(OpCodes.SetLocalS) }

const getOpByType = (writer: CodegenFunctionWriter, type: Type): keyof typeof GetLocalByType => {
  if (type === IntType || type === BoolType) return 'I32'
  else if (type === FloatType) return 'F32'
  else if (type === DoubleType) return 'F64'
  else if (type === RawPointerType) return 'I64'
  else if (type instanceof ParameterizedType || type instanceof ConcreteClassType) return 'S'
  compilerAssert(false, "Unexpected type", { type })
}

const astWriter: AstWriterTable = {
  statements: (writer, ast) => {
    writer.currentScopeIndex ++
    // TODO: Filter voids?
    ast.statements.forEach((expr, i) => {
      writeExpr(writer, expr);
      if (i !== ast.statements.length - 1 && expr.type !== VoidType) {
        writeBytes(writer, OpCodes.Pop, slotSize(writer, expr.type))
        writer.nextLocalSlot -= slotSize(writer, expr.type)
      }
    })

    let numLocalSlots = 0
    // console.log("Prev locals is", writer.locals)
    // console.log("writer.nextLocalSlot", writer.nextLocalSlot)
    let popped = []
    while (writer.locals.length && writer.locals[writer.locals.length - 1].scopeIndex === writer.currentScopeIndex) {
      const sizeSlots = slotSize(writer, writer.locals[writer.locals.length - 1].binding.type)
      numLocalSlots += sizeSlots
      // writer.nextLocalSlot -= sizeSlots
      popped.push(writer.locals[writer.locals.length - 1].binding.name)
      writer.locals.pop()
    }
    compilerAssert(writer.nextLocalSlot >= 0, "Slot mismatch error", { fatal: true, nextLocalSlot: writer.nextLocalSlot })
    // console.log("Popped", popped)
    // console.log("Popped ", numLocalSlots, " now new slot is ", writer.nextLocalSlot)
    // console.log("New locals is", writer.locals)
    if (numLocalSlots > 0 || slotSize(writer, ast.type) > 0) 
      writeBytes(writer, OpCodes.PopResult, numLocalSlots, slotSize(writer, ast.type))

    writer.nextLocalSlot -= numLocalSlots
    
    writer.currentScopeIndex --
  },
  string: (writer, ast) => {
    let index = writer.constants.get(ast.value);
    const uint8 = new TextEncoder().encode(ast.value)
    const lengthZ = uint8.length + 1

    if (index === undefined) {
      index = writer.nextConstantSlot

      writer.nextConstantSlot += lengthZ
      writer.constants.set(ast.value, index)
      let i = 0
      for (; i < uint8.length; i++) 
        writeUint32LittleEndian(writer.constantSlots, writer.constantSlots.length, uint8[i])
      writeUint32LittleEndian(writer.constantSlots, writer.constantSlots.length, 0)
    }

    emitConstant(writer, IntType, uint8.length) // Don't include zero character here
    writeBytes(writer, OpCodes.ConstantS, index, lengthZ)
    writeBytes(writer, OpCodes.Alloc, lengthZ)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  cast: (writer, ast) => {
    writeExpr(writer, ast.expr)
    let opcode = 0
    if (ast.expr.type === IntType && ast.type === FloatType)      opcode = OpCodes.I32_TO_F32
    else if (ast.expr.type === FloatType && ast.type === IntType) opcode = OpCodes.F32_TO_I32
    else compilerAssert(false, "Not implemented yet", { from: ast.expr.type, to: ast.type })
    // Probably the same size
    writeBytes(writer, opcode)
    writer.nextLocalSlot -= slotSize(writer, ast.expr.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.CheckStack, writer.nextLocalSlot)
  },
  binding: (writer, ast) => {
    const local = writer.locals.find(x => x.binding === ast.binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
    compilerAssert(ast.binding.type !== VoidType);
    const op = getOpByType(writer, ast.binding.type)
    GetLocalByType[op].write(writer, local.slot, slotSize(writer, local.binding.type))
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented")
    if (ast.value) writeExpr(writer, ast.value);

    writer.locals.push({ binding: ast.binding, slot: writer.nextLocalSlot - slotSize(writer, ast.binding.type), scopeIndex: writer.currentScopeIndex })
    console.log("let", ast.binding.name, "at", writer.nextLocalSlot - slotSize(writer, ast.binding.type), "size", slotSize(writer, ast.binding.type))
  },
  set: (writer, ast) => {
    const local = writer.locals.find(x => x.binding === ast.binding)
    compilerAssert(local !== undefined);
    writeExpr(writer, ast.value);
    console.log("set", ast.binding.name)
    const op = getOpByType(writer, ast.binding.type)
    SetLocalByType[op].write(writer, local.slot, slotSize(writer, ast.value.type))
    writer.nextLocalSlot -= slotSize(writer, ast.value.type)
  },
  number: (writer, ast) => {
    compilerAssert(ast.type === IntType, "Expected int type", { ast })
    emitConstant(writer, IntType, ast.value)
    writer.nextLocalSlot += slotSize(writer, ast.type);
  },
  bool: (writer, ast) => {
    compilerAssert(ast.type === BoolType, "Expected bool type", { ast })
    emitConstant(writer, BoolType, ast.value ? 1 : 0)
    writer.nextLocalSlot += slotSize(writer, ast.type);
  },
  if: (writer, ast) => {
    writeExpr(writer, ast.expr);
    const patch1 = writeJump(writer, OpCodes.JumpIfFalsePop);
    writer.nextLocalSlot -= slotSize(writer, ast.expr.type)
    writeExpr(writer, ast.trueBody);
    writer.nextLocalSlot -= slotSize(writer, ast.trueBody.type)
    if (ast.falseBody) {
      const patch2 = writeJump(writer, OpCodes.Jump);
      patch1();
      writeExpr(writer, ast.falseBody);
      writer.nextLocalSlot -= slotSize(writer, ast.falseBody.type)
      patch2();
    } else patch1()
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  while: (writer, ast) => {
    const loop = writer.bytecode.length
    writeExpr(writer, ast.condition)
    const patch1 = writeJump(writer, OpCodes.JumpIfFalsePop)
    writer.nextLocalSlot -= slotSize(writer, ast.condition.type)
    writeExpr(writer, ast.body)
    writeBytes(writer, OpCodes.Loop, 0, 0)
    writeLittleEndian16At(writer.bytecode, writer.bytecode.length - 2, writer.bytecode.length - loop)
    patch1()
    compilerAssert(ast.body.type === VoidType);
  },
  break: (writer, ast) => {
    const block = writer.blocks.findLast(x => x.binding === ast.binding)
    compilerAssert(block, "Block expected", { ast, fatal: true })
    const returnSize = ast.expr ? slotSize(writer, ast.expr.type) : 0
    const popSize = writer.nextLocalSlot - block.slotIndex
    if (popSize > 0) writeBytes(writer, OpCodes.PopResult, popSize, returnSize)
    writeBytes(writer, OpCodes.Jump, 0, 0)
    block.patches.push({ location: writer.bytecode.length - 2 })
  },
  and: (writer, ast) => {
    const [a, b] = ast.args;
    compilerAssert(a.type === BoolType || a.type === IntType)
    compilerAssert(b.type === BoolType || b.type === IntType)
    writeExpr(writer, a);
    writer.nextLocalSlot -= slotSize(writer, a.type)
    const patch = writeJump(writer, OpCodes.JumpIfFalse);
    writeBytes(writer, OpCodes.Pop, slotSize(writer, a.type));
    writeExpr(writer, b);
    writer.nextLocalSlot -= slotSize(writer, a.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    patch();
  },
  or: (writer, ast) => {
    compilerAssert(false, "Not implemented")
    const [a, b] = ast.args;
    writeExpr(writer, a);
    const patch1 = writeJump(writer, OpCodes.JumpIfFalse);
    const patch2 = writeJump(writer, OpCodes.Jump);
    patch1();
    writeBytes(writer, OpCodes.Pop, slotSize(writer, a.type));
    writeExpr(writer, b);
    patch2();
  },
  call: (writer, ast) => {
    if (ast.func.name === "print") {
      compilerAssert(ast.args.length === 1, "Print not implemented yet", { ast });
      // compilerAssert(false, "Not implemented 'print'", { ast })
      writeExpr(writer, ast.args[0])

      if (ast.args[0].type === IntType) writeBytes(writer, OpCodes.ToStringI32);
      // else compilerAssert(false, `Unsupported ${ast.args[0].type._type}`);
      writeBytes(writer, OpCodes.Print, 1);
      writer.nextLocalSlot -= slotSize(writer, ast.args[0].type)
      compilerAssert(ast.type === VoidType, "Expected void")
      return;
    }
    compilerAssert(false, "Not supported");
    // TODO: func name
    // params.forEach(writeExpr);
    // writeBytes(OpCodes.Call, params.length);
  },
  list: (writer, ast) => {
    ast.args.forEach(x => writeExpr(writer, x))
    ast.args.forEach(x => writer.nextLocalSlot -= slotSize(writer, x.type))
    writeBytes(writer, OpCodes.List, slotSize(writer, ast.args[0].type), ast.args.length)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },

  usercall: (writer, ast) => {
    const index = writer.writer.functionToIndex.get(ast.binding)
    compilerAssert(index !== undefined, "Expected function");
    // TODO: func name
    ast.args.forEach(x => writeExpr(writer, x));
    ast.args.forEach(expr => writer.nextLocalSlot -= slotSize(writer, expr.type));
    writeBytes(writer, OpCodes.Call, index, ast.args.length);
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  operator: (writer, ast) => {
    writeExpr(writer, ast.args[0]);
    writeExpr(writer, ast.args[1]);
    writeOperator(writer, ast.operator, ast.type);
    writer.nextLocalSlot -= slotSize(writer, ast.args[0].type)
    writer.nextLocalSlot -= slotSize(writer, ast.args[1].type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    // writeBytes(operatorToBytecode(ast.values[0], ast.values[1].type));
  },
  constructor: (writer, ast) => {
    ast.args.forEach(expr => writeExpr(writer, expr))
    if (ast.type.typeInfo.isReferenceType) {
      ast.args.forEach(expr => writer.nextLocalSlot -= slotSize(writer, expr.type))
      writeBytes(writer, OpCodes.Alloc, slotSize(writer, ast.type))
      writer.nextLocalSlot += slotSize(writer, ast.type) // Pointer size
    } else {} // Fields are kept on stack as value type
  },
  field: (writer, ast) => {
    if (!ast.left.type.typeInfo.isReferenceType) {
      // compilerAssert(false, "Field of value type not implemented yet", { type: ast.left.type, field: ast.field, left: ast.left })
      if (ast.left instanceof BindingAst) {
        const binding = ast.left.binding
        const local = writer.locals.find(x => x.binding === binding)
        compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
        console.log("get", local.binding.name, local.slot)
        const op = getOpByType(writer, ast.type)
        const slot = local.slot + getSlotOffset(writer, ast.field)
        GetLocalByType[op].write(writer, slot, slotSize(writer, ast.type))
        writer.nextLocalSlot += slotSize(writer, ast.type)
        writeBytes(writer, OpCodes.CheckStack, writer.nextLocalSlot)
        return
      } else {
        writeExpr(writer, ast.left)
        const fieldSlots = slotSize(writer, ast.type)
        const prevFieldsSize = ast.field.sourceType.typeInfo.fields.slice(0, ast.field.index).reduce((acc, x) => acc + slotSize(writer, x.fieldType), 0)
        const nextFieldsSize = ast.field.sourceType.typeInfo.fields.slice(ast.field.index + 1).reduce((acc, x) => acc + slotSize(writer, x.fieldType), 0)
        if (nextFieldsSize > 0) writeBytes(writer, OpCodes.PopResult, nextFieldsSize, 0) // pop slots after
        writeBytes(writer, OpCodes.PopResult, prevFieldsSize, fieldSlots) // pop slots before and return selected field
        writer.nextLocalSlot -= slotSize(writer, ast.left.type)
        writer.nextLocalSlot += fieldSlots
        writeBytes(writer, OpCodes.CheckStack, writer.nextLocalSlot)
        return
      }
    }
    writeExpr(writer, ast.left)
    writeBytes(writer, OpCodes.GetField, getSlotOffset(writer, ast.field), slotSize(writer, ast.field.fieldType))
    writer.nextLocalSlot -= slotSize(writer, ast.left.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  subscript: (writer, ast) => {
    compilerAssert(ast.left.type instanceof ParameterizedType && ast.left.type.typeConstructor === ListTypeConstructor, "Expected list", { ast })
    compilerAssert(ast.right.type === IntType, "Expected int type", { ast })
    writeExpr(writer, ast.left)
    writeExpr(writer, ast.right)
    writeBytes(writer, OpCodes.SubscriptList, slotSize(writer, ast.type))
    writer.nextLocalSlot -= slotSize(writer, ast.left.type)
    writer.nextLocalSlot -= slotSize(writer, ast.right.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  block: (writer, ast) => {
    writer.blocks.push({ binding: ast.binding, slotIndex: writer.nextLocalSlot, patches: [] })
    writeExpr(writer, ast.body)
    const block = writer.blocks.pop()!
    block.patches.forEach(p => 
      writeLittleEndian16(writer.bytecode, p.location, writer.bytecode.length))
  },
  not: (writer, ast) => {
    writeExpr(writer, ast.expr);
    writeBytes(writer, OpCodes.NotI32);
    writer.nextLocalSlot -= slotSize(writer, ast.expr.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  void: (writer, ast) => {}
};

export const writeFinalBytecode = (globalCompilerState: GlobalCompilerState, outputWriter: FileWriter) => {
  const bytecodeWriter: CodegenWriter = {
    functions: [],
    globalCompilerState,
    functionToIndex: new Map(),
    typeSizes: new Map()
  }
  let index = 0;
  globalCompilerState.compiledFunctions.forEach(func => {
    bytecodeWriter.functionToIndex.set(func.binding, index++)
  })
  const funcWriters = Array.from(globalCompilerState.compiledFunctions.values()).map(func => {
    const funcWriter = writeFinalBytecodeFunction(bytecodeWriter, func)
    return funcWriter
  })

  const bytes: number[] = []
  bytes.push(funcWriters.length)
  for (const f of funcWriters) {
    bytes.push(f.argSlots);
    bytes.push(f.returnSlots);
    bytes.push(f.constantSlots.length);
    bytes.push(f.bytecode.length);
  }
  outputWriter.write(new Uint8Array(bytes))


  for (const f of funcWriters) {
    const output: number[] = []
    f.constantSlots.forEach(c => {
      writeLittleEndian32(output, output.length, c)
    });
    outputWriter.write(new Uint8Array(output))
    outputWriter.write(new Uint8Array(f.bytecode));
  }
  
  return bytecodeWriter
}

function writeLittleEndian32(arr: number[], offset: number, number: number) {
  arr[offset] = number & 0xFF;                   // Least significant byte
  arr[offset + 1] = (number >> 8) & 0xFF;
  arr[offset + 2] = (number >> 16) & 0xFF;
  arr[offset + 3] = (number >> 24) & 0xFF;       // Most significant byte
}
function writeLittleEndian16(arr: number[], offset: number, number: number) {
  arr[offset] = number & 0xFF;            // Write the least significant byte
  arr[offset + 1] = (number >> 8) & 0xFF; // Write the most significant byte
}

const writeFinalBytecodeFunction = (bytecodeWriter: CodegenWriter, func: CompiledFunction) => {
  console.log("\nWriting func", func.functionDefinition.debugName, "\n")
  const funcWriter: CodegenFunctionWriter = {
    writer: bytecodeWriter,
    argSlots: 0,
    returnSlots: 0,
    bytecode: [],
    constants: new Map(),
    constantSlots: [],
    nextConstantSlot: 0,
    blocks: [],
    locals: [],
    currentScopeIndex: 0,
    nextLocalSlot: 0
  }
  funcWriter.argSlots = Object.values(func.concreteTypes).reduce((acc, x) => acc + slotSize(funcWriter, x), 0); // prettier-ignore
  funcWriter.returnSlots = slotSize(funcWriter, func.returnType);

  func.argBindings.forEach((binding, i) => {
    funcWriter.locals.push({ binding, slot: funcWriter.nextLocalSlot, scopeIndex: funcWriter.currentScopeIndex })
    funcWriter.nextLocalSlot += slotSize(funcWriter, binding.type);
  });

  writeExpr(funcWriter, func.body);
  writeBytes(funcWriter, OpCodes.Return);

  // Remove locals including args we pushed before
  let numLocalSlots = 0
  while (funcWriter.locals.length && funcWriter.locals[funcWriter.locals.length - 1].scopeIndex === funcWriter.currentScopeIndex) {
    const sizeSlots = slotSize(funcWriter, funcWriter.locals[funcWriter.locals.length - 1].binding.type)
    numLocalSlots += sizeSlots
    funcWriter.nextLocalSlot -= sizeSlots
    funcWriter.locals.pop()
  }

  // Make sure locals/pops match up
  compilerAssert(funcWriter.locals.length === 0, "Compile error got $x expected 0", { x: funcWriter.locals.length, fatal: true })
  compilerAssert(funcWriter.nextLocalSlot === funcWriter.returnSlots, "Compile error got $x expected 0", { x: funcWriter.nextLocalSlot, fatal: true })

  console.log(funcWriter.writer.typeSizes)

  bytecodeWriter.functions.push(funcWriter);
  return funcWriter
};
