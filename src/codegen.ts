import { externals } from "./compiler_sugar";
import { Ast, AstType, AstWriterTable, Binding, BindingAst, BoolType, CodegenFunctionWriter, CodegenWriter, CompiledFunction, ConcreteClassType, DoubleType, FileWriter, FloatType, GlobalCompilerState, IntType, ListTypeConstructor, ParameterizedType, PrimitiveType, RawPointerType, StringType, Type, TypeField, VoidType, compilerAssert, textColors } from "./defs";

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
  SetField: 16,
  SubscriptList: 17,
  List: 18,
  Alloc: 19,
  ConstantS: 20,
  GetLocalS: 21,
  SetLocalS: 22,
  GetGlobalS: 23,
  SetGlobalS: 24,
  AllocS: 25,
  SubscriptS: 26,
  SetSubscriptS: 27,
  LocalAddress: 28,
  F32_TO_I32: 29,
  I32_TO_F32: 30,
  ExternalCall: 31,
  BitShiftLeft: 32,
  BitShiftRight: 33,
  BitwiseAnd: 34,
  BitwiseOr: 35,
  CheckStack: 36,
  ConstantV: 37,
  ConstantF32: 38,
  ConstantF64: 39,
  ConstantI32: 40,
  ConstantI64: 41,
  GetLocalV: 42,
  GetLocalF32: 43,
  GetLocalF64: 44,
  GetLocalI32: 45,
  GetLocalI64: 46,
  SetLocalV: 47,
  SetLocalF32: 48,
  SetLocalF64: 49,
  SetLocalI32: 50,
  SetLocalI64: 51,
  GetGlobalV: 52,
  GetGlobalF32: 53,
  GetGlobalF64: 54,
  GetGlobalI32: 55,
  GetGlobalI64: 56,
  SetGlobalV: 57,
  SetGlobalF32: 58,
  SetGlobalF64: 59,
  SetGlobalI32: 60,
  SetGlobalI64: 61,
  SubscriptV: 62,
  SubscriptF32: 63,
  SubscriptF64: 64,
  SubscriptI32: 65,
  SubscriptI64: 66,
  SetSubscriptV: 67,
  SetSubscriptF32: 68,
  SetSubscriptF64: 69,
  SetSubscriptI32: 70,
  SetSubscriptI64: 71,
  EqualV: 72,
  EqualF32: 73,
  EqualF64: 74,
  EqualI32: 75,
  EqualI64: 76,
  GreaterV: 77,
  GreaterF32: 78,
  GreaterF64: 79,
  GreaterI32: 80,
  GreaterI64: 81,
  LessV: 82,
  LessF32: 83,
  LessF64: 84,
  LessI32: 85,
  LessI64: 86,
  AddV: 87,
  AddF32: 88,
  AddF64: 89,
  AddI32: 90,
  AddI64: 91,
  SubtractV: 92,
  SubtractF32: 93,
  SubtractF64: 94,
  SubtractI32: 95,
  SubtractI64: 96,
  MultiplyV: 97,
  MultiplyF32: 98,
  MultiplyF64: 99,
  MultiplyI32: 100,
  MultiplyI64: 101,
  DivideV: 102,
  DivideF32: 103,
  DivideF64: 104,
  DivideI32: 105,
  DivideI64: 106,
  NotV: 107,
  NotF32: 108,
  NotF64: 109,
  NotI32: 110,
  NotI64: 111,
  NegateV: 112,
  NegateF32: 113,
  NegateF64: 114,
  NegateI32: 115,
  NegateI64: 116,
  ToStringV: 117,
  ToStringF32: 118,
  ToStringF64: 119,
  ToStringI32: 120,
  ToStringI64: 121,
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

const log = (...args: any[]) => {
  if ((globalThis as any).logger) (globalThis as any).logger.log(...args)
}


const writeBytes = (writer: CodegenFunctionWriter, ...values: number[]) => {
  values.forEach((x) => compilerAssert(x < 2 ** 8, `Expected ${x} < 256`));
  const name = Object.entries(OpCodes).find(x => x[1] === values[0])?.[0]
  const indent = "  ".repeat(Math.max(writer.nextLocalSlot, 0))
  log(textColors.green(String(writer.bytecode.length).padEnd(3, ' ')) + " |" + indent + name, ...values.slice(1))
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

const arrayBuffer = new Uint32Array(2)

function writeFloatLittleEndian(arr: number[], offset: number, number: number) {
  let dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength)
  dataView.setFloat32(0, number, true)
  arr[offset + 0] = arrayBuffer[0]
}
function writeDoubleLittleEndian(arr: number[], offset: number, number: number) {
  let dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength)
  dataView.setFloat64(0, number, true)
  arr[offset + 0] = arrayBuffer[0]
  arr[offset + 1] = arrayBuffer[1]
}
function writeUint32LittleEndian(arr: number[], offset: number, number: number) {
  let dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength)
  dataView.setUint32(0, number, true)
  arr[offset] = arrayBuffer[0]
}
function writeUint64LittleEndian(arr: number[], offset: number, number: number) {
  const left = number % 0x100000000
  const right = Math.floor(number / 0x100000000)
  const arrayBuffer = new Uint32Array(8)
  const dataView = new DataView(arrayBuffer.buffer, arrayBuffer.byteOffset, arrayBuffer.byteLength)
  dataView.setUint32(0, left, true)
  dataView.setUint32(4, right, true)
  arr[offset] = arrayBuffer[0]
  arr[offset + 1] = arrayBuffer[1]
}
const writeTypeAt = (arr: number[], offset: number, type: Type, value: number) => {
  if (type === RawPointerType) {
    writeUint64LittleEndian(arr, offset, value);
    return
  }
  if (type === FloatType) {
    writeFloatLittleEndian(arr, offset, value)
    return
  }
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
  } else if (op === "!=") {
    writeOperator(writer, "==", type);
    writeBytes(writer, OpCodes.NotI32);
    return;
  } else if (op === "&") {
    writeBytes(writer, OpCodes.BitwiseAnd); return
  } else if (op === "|") {
    writeBytes(writer, OpCodes.BitwiseAnd); return
  } else if (op === "<<") {
    writeBytes(writer, OpCodes.BitShiftLeft); return
  } else if (op === ">>") {
    writeBytes(writer, OpCodes.BitShiftRight); return
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

type WriterLike = { writer: CodegenWriter } // Make it easy for callers to pass this
const slotSize = (writer: WriterLike, type: Type): number => {
  if (writer.writer.typeSizes.get(type)) return writer.writer.typeSizes.get(type)!;
  const s = calculateTypeSize(writer, type);
  writer.writer.typeSizes.set(type, s)
  return s
}
const calculateTypeSize = (writer: WriterLike, type: ParameterizedType | ConcreteClassType | PrimitiveType) => {
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

const constantTableByType = (writer: CodegenFunctionWriter, type: Type) => {
  if (type === RawPointerType || type === BoolType) type = IntType // normalise int types
  let byType = writer.constantsByType.get(type)
  if (!byType) { byType = new Map(); writer.constantsByType.set(type, byType) }
  return byType
}
const emitConstant = (writer: CodegenFunctionWriter, type: Type, value: number) => {
  if (type === RawPointerType) {
    // TODO: Cache values by type size
    let index = writer.nextConstantSlot
    writer.nextConstantSlot += slotSize(writer, type)
    writeTypeAt(writer.constantSlots, writer.constantSlots.length, type, value)
    writeBytes(writer, OpCodes.ConstantI64, index)
    return
  }
  
  const byType = constantTableByType(writer, type)
  let index = byType.get(value);
  if (index === undefined) {
    index = writer.nextConstantSlot
    writer.nextConstantSlot += slotSize(writer, type)
    byType.set(value, index)
    writeTypeAt(writer.constantSlots, writer.constantSlots.length, type, value)
  }
  if (type === IntType || type === BoolType) writeBytes(writer, OpCodes.ConstantI32, index)
  else if (type === FloatType) writeBytes(writer, OpCodes.ConstantF32, index)
  else compilerAssert(false, "Constant not implemented for type", { type })
}

const getFieldOp  = (opcode: number) => ({ opcode, write: (writer: CodegenFunctionWriter, slot: number, size: number) => writeBytes(writer, opcode, slot) })
const getFieldOpS = (opcode: number) => ({ opcode, write: (writer: CodegenFunctionWriter, slot: number, size: number) => writeBytes(writer, opcode, slot, size) })
const subscriptOp  = (opcode: number) => ({ opcode, write: (writer: CodegenFunctionWriter, size: number) => writeBytes(writer, opcode) })
const subscriptOpS = (opcode: number) => ({ opcode, write: (writer: CodegenFunctionWriter, size: number) => writeBytes(writer, opcode, size) })
const GetLocalByType     = { I32: getFieldOp(OpCodes.GetLocalI32), I64: getFieldOp(OpCodes.GetLocalI64), F32: getFieldOp(OpCodes.GetLocalF32), F64: getFieldOp(OpCodes.GetLocalF64), S: getFieldOpS(OpCodes.GetLocalS) }
const SetLocalByType     = { I32: getFieldOp(OpCodes.SetLocalI32), I64: getFieldOp(OpCodes.SetLocalI64), F32: getFieldOp(OpCodes.SetLocalF32), F64: getFieldOp(OpCodes.SetLocalF64), S: getFieldOpS(OpCodes.SetLocalS) }
const GetGlobalByType    = { I32: getFieldOp(OpCodes.GetGlobalI32), I64: getFieldOp(OpCodes.GetGlobalI64), F32: getFieldOp(OpCodes.GetGlobalF32), F64: getFieldOp(OpCodes.GetGlobalF64), S: getFieldOpS(OpCodes.GetGlobalS) }
const SetGlobalByType    = { I32: getFieldOp(OpCodes.SetGlobalI32), I64: getFieldOp(OpCodes.SetGlobalI64), F32: getFieldOp(OpCodes.SetGlobalF32), F64: getFieldOp(OpCodes.SetGlobalF64), S: getFieldOpS(OpCodes.SetGlobalS) }
const SubscriptByType    = { I32: subscriptOp(OpCodes.SubscriptI32), I64: subscriptOp(OpCodes.SubscriptI64), F32: subscriptOp(OpCodes.SubscriptF32), F64: subscriptOp(OpCodes.SubscriptF64), S: subscriptOpS(OpCodes.SubscriptS) }
const SetSubscriptByType = { I32: subscriptOp(OpCodes.SetSubscriptI32), I64: subscriptOp(OpCodes.SetSubscriptI64), F32: subscriptOp(OpCodes.SetSubscriptF32), F64: subscriptOp(OpCodes.SetSubscriptF64), S: subscriptOpS(OpCodes.SetSubscriptS) }

const getOpByType = (writer: CodegenFunctionWriter, type: Type): keyof typeof GetLocalByType => {
  if (type === IntType || type === BoolType) return 'I32'
  else if (type === FloatType) return 'F32'
  else if (type === DoubleType) return 'F64'
  else if (type === RawPointerType) return 'I64'
  else if (type === StringType) return 'S'
  else if (type instanceof ParameterizedType || type instanceof ConcreteClassType) return 'S'
  compilerAssert(false, "Unexpected type", { type })
}

const astWriter: AstWriterTable<CodegenFunctionWriter> = {
  statements: (writer, ast) => {
    writer.currentScopeIndex ++
    // TODO: Filter voids?
    ast.statements.forEach((expr, i) => {
      writeExpr(writer, expr)
      const toPop = i !== ast.statements.length - 1 || ast.type === VoidType
      if (expr.type !== VoidType && toPop) {
        writeBytes(writer, OpCodes.Pop, slotSize(writer, expr.type))
        writer.nextLocalSlot -= slotSize(writer, expr.type)
      }
    })

    let numLocalSlots = 0
    // log("Prev locals is", writer.locals)
    // log("writer.nextLocalSlot", writer.nextLocalSlot)
    let popped = []
    while (writer.locals.length && writer.locals[writer.locals.length - 1].scopeIndex === writer.currentScopeIndex) {
      const sizeSlots = slotSize(writer, writer.locals[writer.locals.length - 1].binding.type)
      numLocalSlots += sizeSlots
      // writer.nextLocalSlot -= sizeSlots
      popped.push(writer.locals[writer.locals.length - 1].binding.name)
      writer.locals.pop()
    }
    compilerAssert(writer.nextLocalSlot >= 0, "Slot mismatch error", { fatal: true, nextLocalSlot: writer.nextLocalSlot })
    // log("Popped", popped)
    // log("Popped ", numLocalSlots, " now new slot is ", writer.nextLocalSlot)
    // log("New locals is", writer.locals)
    if (numLocalSlots > 0) 
      writeBytes(writer, OpCodes.PopResult, numLocalSlots, slotSize(writer, ast.type))

    writer.nextLocalSlot -= numLocalSlots
    
    writer.currentScopeIndex --
  },
  string: (writer, ast) => {
    const byType = constantTableByType(writer, StringType)
    let index = byType.get(ast.value);
    const uint8 = new TextEncoder().encode(ast.value)
    const lengthZ = uint8.length + 1

    if (index === undefined) {
      index = writer.nextConstantSlot

      writer.nextConstantSlot += lengthZ
      byType.set(ast.value, index)
      let i = 0
      for (; i < uint8.length; i++) 
        writeUint32LittleEndian(writer.constantSlots, writer.constantSlots.length, uint8[i])
      writeUint32LittleEndian(writer.constantSlots, writer.constantSlots.length, 0)
    }

    emitConstant(writer, IntType, uint8.length) // Don't include zero character here
    writeBytes(writer, OpCodes.ConstantS, index, lengthZ)
    writeBytes(writer, OpCodes.Alloc, lengthZ)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.CheckStack, writer.nextLocalSlot)
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
    compilerAssert(ast.binding.type !== VoidType);
    if (writer.writer.globals.has(ast.binding)) {
      const op = getOpByType(writer, ast.binding.type)
      GetGlobalByType[op].write(writer, writer.writer.globals.get(ast.binding)!, slotSize(writer, ast.binding.type))
      writer.nextLocalSlot += slotSize(writer, ast.type)
      return
    }
    const local = writer.locals.find(x => x.binding === ast.binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
    const op = getOpByType(writer, ast.binding.type)
    GetLocalByType[op].write(writer, local.slot, slotSize(writer, local.binding.type))
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  let: (writer, ast) => {
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented", { ast })
    if (ast.value) writeExpr(writer, ast.value);

    writer.locals.push({ binding: ast.binding, slot: writer.nextLocalSlot - slotSize(writer, ast.binding.type), scopeIndex: writer.currentScopeIndex })
    // log("let", ast.binding.name, "at", writer.nextLocalSlot - slotSize(writer, ast.binding.type), "size", slotSize(writer, ast.binding.type))
  },
  set: (writer, ast) => {
    if (writer.writer.globals.has(ast.binding)) {
      writeExpr(writer, ast.value);
      const op = getOpByType(writer, ast.binding.type)
      SetGlobalByType[op].write(writer, writer.writer.globals.get(ast.binding)!, slotSize(writer, ast.binding.type))
      writer.nextLocalSlot -= slotSize(writer, ast.binding.type)
      return
    }
    const local = writer.locals.find(x => x.binding === ast.binding)
    compilerAssert(local !== undefined);
    writeExpr(writer, ast.value);
    // log("set", ast.binding.name)
    const op = getOpByType(writer, ast.binding.type)
    SetLocalByType[op].write(writer, local.slot, slotSize(writer, ast.value.type))
    writer.nextLocalSlot -= slotSize(writer, ast.value.type)
  },
  number: (writer, ast) => {
    compilerAssert(ast.type === IntType || ast.type === FloatType || ast.type === DoubleType, "Expected number type got $type", { ast, type: ast.type })
    emitConstant(writer, ast.type, ast.value)
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
    if (ast.expr) writeExpr(writer, ast.expr)
    if (popSize > 0 || returnSize > 0) writeBytes(writer, OpCodes.PopResult, popSize, returnSize)
    writer.nextLocalSlot -= returnSize
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
    // compilerAssert(false, "Not implemented", { ast })
    const [a, b] = ast.args;
    writeExpr(writer, a);
    const patch1 = writeJump(writer, OpCodes.JumpIfFalse);
    const patch2 = writeJump(writer, OpCodes.Jump);
    patch1();
    writeBytes(writer, OpCodes.Pop, slotSize(writer, a.type));
    writer.nextLocalSlot -= slotSize(writer, a.type)
    writeExpr(writer, b);
    patch2();
  },
  call: (writer, ast) => {
    if (ast.func.name === "print") {
      compilerAssert(ast.args.length === 1, "Print not implemented yet", { ast });
      // compilerAssert(false, "Not implemented 'print'", { ast })
      writeExpr(writer, ast.args[0])

      if (ast.args[0].type === IntType || ast.args[0].type === BoolType) writeBytes(writer, OpCodes.ToStringI32);
      else if (ast.args[0].type === FloatType) writeBytes(writer, OpCodes.ToStringF32);
      else if (ast.args[0].type === DoubleType) writeBytes(writer, OpCodes.ToStringF64);
      else if (ast.args[0].type === RawPointerType) writeBytes(writer, OpCodes.ToStringI64);
      else if (ast.args[0].type === StringType) {}
      else compilerAssert(false, `Not implemented`, { type: ast.args[0].type });
      writeBytes(writer, OpCodes.Print, 1);
      writer.nextLocalSlot -= slotSize(writer, ast.args[0].type)
      compilerAssert(ast.type === VoidType, "Expected void")
      return;
    }

    if (ast.func === externals.malloc) {
      compilerAssert(ast.args.length == 1 && ast.args[0].type === IntType, "Expected int arg", { ast })
      emitConstant(writer, RawPointerType, 0)
      writeExpr(writer, ast.args[0])
      writeBytes(writer, OpCodes.AllocS)
      writer.nextLocalSlot -= slotSize(writer, IntType)
      writer.nextLocalSlot += slotSize(writer, ast.type) // Pointer size
      return
    }
    if (ast.func === externals.realloc) {
      compilerAssert(ast.args.length == 2 && ast.args[0].type === RawPointerType && ast.args[1].type === IntType, "Expected int arg", { ast })
      writeExpr(writer, ast.args[0])
      writeExpr(writer, ast.args[1])
      writeBytes(writer, OpCodes.AllocS)
      writer.nextLocalSlot -= slotSize(writer, ast.args[0].type)
      writer.nextLocalSlot -= slotSize(writer, ast.args[1].type)
      writer.nextLocalSlot += slotSize(writer, ast.type) // Pointer size
      return
    }
    const externalIndex = Array.from(Object.values(externals)).findIndex(x => x === ast.func)
    compilerAssert(externalIndex > -1, `External function not supported in codegen: ${ast.func.name}`);

    ast.args.forEach(x => writeExpr(writer, x))
    writeBytes(writer, OpCodes.ExternalCall, externalIndex)
    ast.args.forEach(x => writer.nextLocalSlot -= slotSize(writer, x.type))
    writer.nextLocalSlot += slotSize(writer, ast.type)
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
    writeBytes(writer, OpCodes.Call, index, ast.args.length);
    ast.args.forEach(expr => writer.nextLocalSlot -= slotSize(writer, expr.type));
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  operator: (writer, ast) => {
    writeExpr(writer, ast.args[0]);
    writeExpr(writer, ast.args[1]);
    writeOperator(writer, ast.operator, ast.type);
    writer.nextLocalSlot -= slotSize(writer, ast.args[0].type)
    writer.nextLocalSlot -= slotSize(writer, ast.args[1].type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  defaultcons: (writer, ast) => {
    const recur = (type: Type) => {
      if (type === RawPointerType) emitConstant(writer, RawPointerType, 0)
      else if (type === IntType) emitConstant(writer, IntType, 0)
      else if (type === BoolType) emitConstant(writer, BoolType, 0)
      else if (type === FloatType) emitConstant(writer, FloatType, 0)
      else if (type.typeInfo.isReferenceType) emitConstant(writer, RawPointerType, 0)
      else {
        type.typeInfo.fields.forEach(field => {recur(field.fieldType)})
      }
    }
    recur(ast.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  constructor: (writer, ast) => {
    // TODO: Split this into two ASTs, a value type literal and an alloc
    compilerAssert(ast.args.length > 0 && ast.args.length === ast.type.typeInfo.fields.length, "Not implemented")
    ast.args.forEach(expr => writeExpr(writer, expr))
    if (ast.type.typeInfo.isReferenceType) {
      const fieldSize = ast.args.reduce((acc, expr) => acc + slotSize(writer, expr.type), 0)
      writer.nextLocalSlot -= fieldSize
      writeBytes(writer, OpCodes.Alloc, fieldSize)
      writer.nextLocalSlot += slotSize(writer, ast.type) // Pointer size
      writeBytes(writer, OpCodes.CheckStack, writer.nextLocalSlot)
    } else {} // Fields are kept on stack as value type
  },
  valuefield: (writer, ast) => {
    // TODO: Another ast where it is a value type but left is not a BindingAst
    const binding = ast.left.binding
    const local = writer.locals.find(x => x.binding === binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
    const slot = ast.fieldPath.reduce((acc, field) => acc + getSlotOffset(writer, field), local.slot)
    log("get", local.binding.name, ast.fieldPath.map(x => x.name), slot)
    const op = getOpByType(writer, ast.type)
    GetLocalByType[op].write(writer, slot, slotSize(writer, ast.type))
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.CheckStack, writer.nextLocalSlot)
  },
  field: (writer, ast) => {
    if (ast.left.type.typeInfo.isReferenceType) {
      writeExpr(writer, ast.left)
      writeBytes(writer, OpCodes.GetField, getSlotOffset(writer, ast.field), slotSize(writer, ast.field.fieldType))
      writer.nextLocalSlot -= slotSize(writer, ast.left.type)
      writer.nextLocalSlot += slotSize(writer, ast.type)
      return
    }
    // TODO: Specific test for this, it does already happen in some tests
    writeExpr(writer, ast.left)
    const fieldSlots = slotSize(writer, ast.type)
    const prevFieldsSize = ast.field.sourceType.typeInfo.fields.slice(0, ast.field.index).reduce((acc, x) => acc + slotSize(writer, x.fieldType), 0)
    const nextFieldsSize = ast.field.sourceType.typeInfo.fields.slice(ast.field.index + 1).reduce((acc, x) => acc + slotSize(writer, x.fieldType), 0)
    if (nextFieldsSize > 0) writeBytes(writer, OpCodes.PopResult, nextFieldsSize, 0) // pop slots after
    writeBytes(writer, OpCodes.PopResult, prevFieldsSize, fieldSlots) // pop slots before and return selected field
    writer.nextLocalSlot -= slotSize(writer, ast.left.type)
    writer.nextLocalSlot += fieldSlots
    writeBytes(writer, OpCodes.CheckStack, writer.nextLocalSlot)
  },
  setvaluefield: (writer, ast) => {
    const binding = ast.left.binding
    const local = writer.locals.find(x => x.binding === binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
    let slot = local.slot
    ast.fieldPath.forEach(field => {slot += getSlotOffset(writer, field)})
    writeExpr(writer, ast.value)
    const op = getOpByType(writer, ast.value.type)
    SetLocalByType[op].write(writer, slot, slotSize(writer, ast.value.type))
    writer.nextLocalSlot -= slotSize(writer, ast.value.type)
  },
  setfield: (writer, ast) => {
    if (ast.left.type.typeInfo.isReferenceType) {
      writeExpr(writer, ast.value)
      writeExpr(writer, ast.left) // Pointer goes last
      writeBytes(writer, OpCodes.SetField, getSlotOffset(writer, ast.field), slotSize(writer, ast.field.fieldType))
      writer.nextLocalSlot -= slotSize(writer, ast.left.type)
      writer.nextLocalSlot -= slotSize(writer, ast.value.type)
      return
    }
    compilerAssert(false, "Not implemented", { ast }) // TODO: Tests for this
  },
  subscript: (writer, ast) => {
    if (ast.left.type === RawPointerType) {
      writeExpr(writer, ast.left)
      writeExpr(writer, ast.right)
      const op = getOpByType(writer, ast.type)
      SubscriptByType[op].write(writer, slotSize(writer, ast.type))
      writer.nextLocalSlot -= slotSize(writer, ast.left.type)
      writer.nextLocalSlot -= slotSize(writer, ast.right.type)
      writer.nextLocalSlot += slotSize(writer, ast.type)
      return
    }
    compilerAssert(ast.left.type instanceof ParameterizedType && ast.left.type.typeConstructor === ListTypeConstructor, "Expected list", { ast })
    compilerAssert(ast.right.type === IntType, "Expected int type", { ast })
    writeExpr(writer, ast.left)
    writeExpr(writer, ast.right)
    writeBytes(writer, OpCodes.SubscriptList, slotSize(writer, ast.type))
    writer.nextLocalSlot -= slotSize(writer, ast.left.type)
    writer.nextLocalSlot -= slotSize(writer, ast.right.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  setsubscript: (writer, ast) => {
    compilerAssert(ast.left.type === RawPointerType, "Not implemented")
    writeExpr(writer, ast.left)
    writeExpr(writer, ast.right)
    writeExpr(writer, ast.value)
    const op = getOpByType(writer, ast.value.type)
    SetSubscriptByType[op].write(writer, slotSize(writer, ast.value.type))
    writer.nextLocalSlot -= slotSize(writer, ast.left.type)
    writer.nextLocalSlot -= slotSize(writer, ast.right.type)
    writer.nextLocalSlot -= slotSize(writer, ast.value.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  deref: (writer, ast) => {
    const binding = ast.left.binding
    const local = writer.locals.find(x => x.binding === binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals })
    const slot = ast.fieldPath.reduce((acc, field) => acc + getSlotOffset(writer, field), local.slot)
    const size = slotSize(writer, ast.type)
    const op = getOpByType(writer, RawPointerType)
    GetLocalByType[op].write(writer, local.slot, slotSize(writer, RawPointerType))
    writer.nextLocalSlot += size
    writeBytes(writer, OpCodes.GetField, slot, size)
  },
  setderef: (writer, ast) => {
    const binding = ast.left.binding
    const local = writer.locals.find(x => x.binding === binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals })
    const slot = ast.fieldPath.reduce((acc, field) => acc + getSlotOffset(writer, field), local.slot)
    const size = slotSize(writer, ast.value.type)
    writeExpr(writer, ast.value)
    const op = getOpByType(writer, RawPointerType)
    GetLocalByType[op].write(writer, local.slot, slotSize(writer, RawPointerType))
    writer.nextLocalSlot -= slotSize(writer, ast.value.type)
    writeBytes(writer, OpCodes.SetField, slot, size)
  },
  block: (writer, ast) => {
    writer.blocks.push({ binding: ast.binding, slotIndex: writer.nextLocalSlot, patches: [] })
    writeExpr(writer, ast.body)
    const block = writer.blocks.pop()!
    block.patches.forEach(p => 
      writeLittleEndian16(writer.bytecode, p.location, writer.bytecode.length - p.location - 2))
  },
  not: (writer, ast) => {
    writeExpr(writer, ast.expr)
    writeBytes(writer, OpCodes.NotI32)
    writer.nextLocalSlot -= slotSize(writer, ast.expr.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  return: (writer, ast) => {
    if (ast.expr) {
      writeExpr(writer, ast.expr)
      writer.nextLocalSlot -= slotSize(writer, ast.expr.type)
    }
    writeBytes(writer, OpCodes.Return)
  },
  address: (writer, ast) => {
    const binding = ast.binding
    const local = writer.locals.find(x => x.binding === binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
    writeBytes(writer, OpCodes.LocalAddress, local.slot)
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  void: (writer, ast) => {}
};

export const writeFinalBytecode = (globalCompilerState: GlobalCompilerState, outputWriter: FileWriter) => {
  const bytecodeWriter: CodegenWriter = {
    functions: [],
    globalCompilerState,
    functionToIndex: new Map(),
    typeSizes: new Map(),
    globals: new Map(),
    nextGlobalSlot: 0,
  }

  bytecodeWriter.typeSizes.set(VoidType, 0)
  bytecodeWriter.typeSizes.set(IntType, 1)
  bytecodeWriter.typeSizes.set(BoolType, 1)
  bytecodeWriter.typeSizes.set(FloatType, 1)
  bytecodeWriter.typeSizes.set(RawPointerType, POINTER_SIZE)

  globalCompilerState.globalLets.forEach(globalLet => {
    bytecodeWriter.globals.set(globalLet.binding, bytecodeWriter.nextGlobalSlot)
    bytecodeWriter.nextGlobalSlot += slotSize({ writer: bytecodeWriter }, globalLet.binding.type)
  })
  
  let index = 0;
  globalCompilerState.compiledFunctions.forEach(func => {
    bytecodeWriter.functionToIndex.set(func.binding, index++)
  })
  const funcWriters = Array.from(globalCompilerState.compiledFunctions.values()).map(func => {
    const funcWriter = writeFinalBytecodeFunction(bytecodeWriter, func)
    return funcWriter
  })

  log('\n\n', bytecodeWriter.typeSizes)

  const bytes: number[] = []
  compilerAssert(bytecodeWriter.nextGlobalSlot < (1 << 16), "Too many global slots")
  writeLittleEndian16At(bytes, bytes.length, bytecodeWriter.nextGlobalSlot)

  compilerAssert(funcWriters.length < (1 << 16), "Too many functions")
  writeLittleEndian16At(bytes, bytes.length, funcWriters.length)

  compilerAssert(bytecodeWriter.nextGlobalSlot < (1 << 8) && bytecodeWriter.nextGlobalSlot < (1 << 8), "TODO: implement instructions that can actually read these")

  for (const f of funcWriters) {
    compilerAssert(f.argSlots < (1 << 8), "Too many arg slots")
    bytes.push(f.argSlots)
    compilerAssert(f.returnSlots < (1 << 8), "Too many return slots")
    bytes.push(f.returnSlots)

    compilerAssert(f.bytecode.length < (1 << 16), "Too many bytecodes")
    writeLittleEndian16At(bytes, bytes.length, f.constantSlots.length)
    
    compilerAssert(f.constantSlots.length < (1 << 16), "Too many constants")
    writeLittleEndian16At(bytes, bytes.length, f.bytecode.length)
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
  log("\nWriting func", func.functionDefinition.debugName, "\n")
  const funcWriter: CodegenFunctionWriter = {
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
  }

  funcWriter.argSlots = Object.values(func.concreteTypes).reduce((acc, concreteType, i) => {
    const storageType = func.functionDefinition.params[i].storage === 'ref' ? RawPointerType : concreteType
    return acc + slotSize(funcWriter, storageType)
  }, 0)
  funcWriter.returnSlots = slotSize(funcWriter, func.returnType);

  func.argBindings.forEach((binding, i) => {
    funcWriter.locals.push({ binding, slot: funcWriter.nextLocalSlot, scopeIndex: funcWriter.currentScopeIndex })
    compilerAssert(binding.type === func.concreteTypes[i], "??")
    const storageType = func.functionDefinition.params[i].storage === 'ref' ? RawPointerType : binding.type
    funcWriter.nextLocalSlot += slotSize(funcWriter, storageType);
  });

  writeExpr(funcWriter, func.body);
  writeBytes(funcWriter, OpCodes.Return);

  // Remove locals including args we pushed before
  let numLocalSlots = 0
  while (funcWriter.locals.length && funcWriter.locals[funcWriter.locals.length - 1].scopeIndex === funcWriter.currentScopeIndex) {
    const binding = funcWriter.locals[funcWriter.locals.length - 1].binding
    const storageType = binding.storage === 'ref' ? RawPointerType : binding.type
    const sizeSlots = slotSize(funcWriter, storageType)
    numLocalSlots += sizeSlots
    funcWriter.nextLocalSlot -= sizeSlots
    funcWriter.locals.pop()
  }

  // Make sure locals/pops match up
  compilerAssert(funcWriter.locals.length === 0, "Compile error got $x expected 0", { x: funcWriter.locals.length, fatal: true })
  compilerAssert(funcWriter.nextLocalSlot === funcWriter.returnSlots, "Compile error got $got expected $expected", { got: funcWriter.nextLocalSlot, expected: funcWriter.returnSlots, fatal: true })

  bytecodeWriter.functions.push(funcWriter);
  return funcWriter
};
