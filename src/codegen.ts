import { Ast, AstWriterTable, Binding, BindingAst, BoolType, CodegenFunctionWriter, CodegenWriter, CompiledFunction, ConcreteClassType, DoubleType, ExternalTypeConstructor, FileWriter, FloatType, GlobalCompilerState, IntType, ListTypeConstructor, ParameterizedType, PrimitiveType, RawPointerType, Type, TypeField, VoidType, compilerAssert } from "./defs";

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
  GetLocalS: 19,
  ConstantV: 20,
  ConstantF32: 21,
  ConstantF64: 22,
  ConstantI32: 23,
  ConstantI64: 24,
  GetLocalV: 25,
  GetLocalF32: 26,
  GetLocalF64: 27,
  GetLocalI32: 28,
  GetLocalI64: 29,
  SetLocalV: 30,
  SetLocalF32: 31,
  SetLocalF64: 32,
  SetLocalI32: 33,
  SetLocalI64: 34,
  EqualV: 35,
  EqualF32: 36,
  EqualF64: 37,
  EqualI32: 38,
  EqualI64: 39,
  GreaterV: 40,
  GreaterF32: 41,
  GreaterF64: 42,
  GreaterI32: 43,
  GreaterI64: 44,
  LessV: 45,
  LessF32: 46,
  LessF64: 47,
  LessI32: 48,
  LessI64: 49,
  AddV: 50,
  AddF32: 51,
  AddF64: 52,
  AddI32: 53,
  AddI64: 54,
  SubtractV: 55,
  SubtractF32: 56,
  SubtractF64: 57,
  SubtractI32: 58,
  SubtractI64: 59,
  MultiplyV: 60,
  MultiplyF32: 61,
  MultiplyF64: 62,
  MultiplyI32: 63,
  MultiplyI64: 64,
  DivideV: 65,
  DivideF32: 66,
  DivideF64: 67,
  DivideI32: 68,
  DivideI64: 69,
  NotV: 70,
  NotF32: 71,
  NotF64: 72,
  NotI32: 73,
  NotI64: 74,
  NegateV: 75,
  NegateF32: 76,
  NegateF64: 77,
  NegateI32: 78,
  NegateI64: 79,
  ToStringV: 80,
  ToStringF32: 81,
  ToStringF64: 82,
  ToStringI32: 83,
  ToStringI64: 84,
};

const POINTER_SIZE = 2;

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
  console.log(writer.bytecode.length, '  ', name, ...values.slice(1))
  writer.bytecode.push(...values);
};
const writeJump = (writer: CodegenFunctionWriter, type: number) => {
  writeBytes(writer, type, 0, 0);
  const jump = writer.bytecode.length;
  return () => {
    console.log("Patch", jump - 2)
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
  compilerAssert(type === IntType);
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
  if (op === "or") {
    compilerAssert(type === IntType);
    writeBytes(writer, OpCodes.Or);
  } else if (op === "and") {
    compilerAssert(type === IntType);
    writeBytes(writer, OpCodes.And);
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
  if (type === RawPointerType) return POINTER_SIZE
  if (type instanceof ParameterizedType || type instanceof ConcreteClassType) {
    if (writer.writer.typeSizes.get(type)) return writer.writer.typeSizes.get(type)!;
    const s = calculateTypeSize(writer, type);
    writer.writer.typeSizes.set(type, s)
    return s
  }
  compilerAssert(false, "Unexpected type $type", { type });
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

const astWriter: AstWriterTable = {
  statements: (writer, ast) => {
    writer.currentScopeIndex ++
    // TODO: Filter voids?
    ast.statements.forEach((expr, i) => {
      writeExpr(writer, expr);
      if (i !== ast.statements.length - 1 && expr.type !== VoidType)
        writeBytes(writer, OpCodes.Pop, slotSize(writer, expr.type));
    })

    let numLocalSlots = 0
    while (writer.locals.length && writer.locals[writer.locals.length - 1].scopeIndex === writer.currentScopeIndex) {
      const sizeSlots = slotSize(writer, writer.locals[writer.locals.length - 1].binding.type)
      numLocalSlots += sizeSlots
      writer.nextLocalSlot -= sizeSlots
      writer.locals.pop()
    }
    writeBytes(writer, OpCodes.PopResult, numLocalSlots, slotSize(writer, ast.type))
    console.log("     After pop nextlocal is ", writer.nextLocalSlot)
    
    writer.currentScopeIndex --
  },
  string: (writer, ast) => {
    compilerAssert(false, "Not implemented string")
  },
  binding: (writer, ast) => {
    const local = writer.locals.find(x => x.binding === ast.binding)
    compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
    compilerAssert(ast.binding.type !== VoidType);
    // if (index === undefined) index = 69; // closure variables
    // compilerAssert(index !== undefined, `Expected local ${ast.values[0].name}`);
    if (ast.binding.type === IntType)              writeBytes(writer, OpCodes.GetLocalI32, local.slot);
    else if (ast.binding.type instanceof ParameterizedType || ast.binding.type instanceof ConcreteClassType) {
      console.log('get', ast.binding.name, local.slot)
      writeBytes(writer, OpCodes.GetLocalS, local.slot, slotSize(writer, local.binding.type));
    } else compilerAssert(false, "Unexpected type", { type: ast.binding.type })
    writer.nextLocalSlot += slotSize(writer, ast.type)
    // console.log(ast);
  },
  let: (writer, ast) => {
    // console.log('writing binding local')
    // const [binding, type, value] = ast.values;
    
    // TODO: No value should zero initialize?
    compilerAssert(ast.value, "Not implemented")
    if (ast.value) writeExpr(writer, ast.value);

    console.log("    let", ast.binding.name, writer.nextLocalSlot - slotSize(writer, ast.binding.type))
    writer.locals.push({ binding: ast.binding, slot: writer.nextLocalSlot - slotSize(writer, ast.binding.type), scopeIndex: writer.currentScopeIndex })
    // writer.localsMap.set(ast.binding, writer.nextLocalSlot);
    // writer.nextLocalSlot += slotSize(writer, ast.binding.type);
  },
  set: (writer, ast) => {
    compilerAssert(ast.binding.type === IntType, `Not implemented type $type`, { type: ast.binding.type });
    // const index = writer.localsMap.get(ast.binding);
    const local = writer.locals.find(x => x.binding === ast.binding)
    compilerAssert(local !== undefined);
    writeExpr(writer, ast.value);
    writer.nextLocalSlot -= slotSize(writer, ast.value.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.SetLocalI32, local.slot);
  },
  number: (writer, ast) => {
    let index = writer.constants.get(ast.value);
    if (index === undefined) {
      index = writer.nextConstantSlot;
      writer.nextConstantSlot += slotSize(writer, ast.type);
      writer.constants.set(ast.value, index);
      console.log("Adding constant", index, ast.value)
      writeTypeAt(writer.constantSlots, writer.constantSlots.length, ast.type, ast.value);
      console.log(writer.constantSlots)
    }
    writeBytes(writer, OpCodes.ConstantI32, index);
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
      patch2();
      writer.nextLocalSlot -= slotSize(writer, ast.falseBody.type)
    } else patch1()
    writer.nextLocalSlot += slotSize(writer, ast.type)
  },
  while: (writer, ast) => {
    const loop = writer.bytecode.length
    writeExpr(writer, ast.condition)
    writer.nextLocalSlot -= slotSize(writer, ast.condition.type)
    const patch1 = writeJump(writer, OpCodes.JumpIfFalsePop)
    writeExpr(writer, ast.body)
    writeBytes(writer, OpCodes.Loop, 0, 0)
    writeLittleEndian16At(writer.bytecode, writer.bytecode.length - 2, writer.bytecode.length - loop)
    patch1()
    compilerAssert(ast.body.type === VoidType);
  },
  and: (writer, ast) => {
    const [a, b] = ast.args;
    writeExpr(writer, a);
    const patch = writeJump(writer, OpCodes.JumpIfFalse);
    writeBytes(writer, OpCodes.Pop, slotSize(writer, a.type));
    writeExpr(writer, b);
    patch();
  },
  or: (writer, ast) => {
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
      compilerAssert(ast.args.length === 1);
      // compilerAssert(false, "Not implemented 'print'", { ast })
      writeExpr(writer, ast.args[0])
      writer.nextLocalSlot -= slotSize(writer, ast.args[0].type)

      if (ast.args[0].type === IntType) writeBytes(writer, OpCodes.ToStringI32);
      // else compilerAssert(false, `Unsupported ${ast.args[0].type._type}`);
      writeBytes(writer, OpCodes.Print, 1);
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
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.List, slotSize(writer, ast.args[0].type), ast.args.length)
  },

  usercall: (writer, ast) => {
    const index = writer.writer.functionToIndex.get(ast.binding)
    compilerAssert(index !== undefined, "Expected function");
    // TODO: func name
    ast.args.forEach(x => writeExpr(writer, x));
    writeBytes(writer, OpCodes.Call, index, ast.args.length);
  },
  operator: (writer, ast) => {
    writeExpr(writer, ast.args[0]);
    writeExpr(writer, ast.args[1]);
    writer.nextLocalSlot -= slotSize(writer, ast.args[0].type)
    writer.nextLocalSlot -= slotSize(writer, ast.args[1].type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    // writeBytes(operatorToBytecode(ast.values[0], ast.values[1].type));
    writeOperator(writer, ast.operator, ast.type);
  },
  constructor: (writer, ast) => {
    ast.args.forEach(expr => writeExpr(writer, expr))
    compilerAssert(ast.args.reduce((acc,x) => acc + slotSize(writer, x.type), 0) === slotSize(writer, ast.type), "Unexpected type mismatch")

    const s = writer.nextLocalSlot
    ast.args.forEach(expr => writer.nextLocalSlot -= slotSize(writer, expr.type))
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.Alloc, slotSize(writer, ast.type))
    console.log(`    next slots is `, writer.nextLocalSlot, `(+${writer.nextLocalSlot - s})`)
  },
  field: (writer, ast) => {
    writeExpr(writer, ast.left)
    if (!ast.left.type.typeInfo.isReferenceType) {
      // compilerAssert(false, "Field of value type not implemented yet", { type: ast.left.type, field: ast.field, left: ast.left })
      if (ast.left instanceof BindingAst) {
        const binding = ast.left.binding
        const local = writer.locals.find(x => x.binding === binding)
        compilerAssert(local !== undefined, "Expected binding", { ast, locals: writer.locals });
        if (ast.type === IntType) {
          writeBytes(writer, OpCodes.GetLocalI32, local.slot + getSlotOffset(writer, ast.field));
          writer.nextLocalSlot -= slotSize(writer, ast.left.type)
          writer.nextLocalSlot += slotSize(writer, ast.type)
          return
        }
        else if (ast.type instanceof ParameterizedType || ast.type instanceof ConcreteClassType) {
          writer.nextLocalSlot -= slotSize(writer, ast.left.type)
          writer.nextLocalSlot += slotSize(writer, ast.type)
          writeBytes(writer, OpCodes.GetLocalS, local.slot + getSlotOffset(writer, ast.field), slotSize(writer, ast.field.fieldType));
          return
        } else compilerAssert(false, "Unexpected type", { type: binding.type })
      }
      compilerAssert(false, "Not implemented yet", { ast })
    }
    writer.nextLocalSlot -= slotSize(writer, ast.left.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.GetField, getSlotOffset(writer, ast.field), slotSize(writer, ast.field.fieldType))
  },
  subscript: (writer, ast) => {
    compilerAssert(ast.left.type instanceof ParameterizedType && ast.left.type.typeConstructor === ListTypeConstructor, "Expected list", { ast })
    compilerAssert(ast.right.type === IntType, "Expected int type", { ast })
    writeExpr(writer, ast.left)
    writeExpr(writer, ast.right)
    writer.nextLocalSlot -= slotSize(writer, ast.left.type)
    writer.nextLocalSlot -= slotSize(writer, ast.right.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.SubscriptList, slotSize(writer, ast.type))
  },
  block: (writer, ast) => {
    writeExpr(writer, ast.body)
  },
  not: (writer, ast) => {
    writeExpr(writer, ast.expr);
    writer.nextLocalSlot -= slotSize(writer, ast.expr.type)
    writer.nextLocalSlot += slotSize(writer, ast.type)
    writeBytes(writer, OpCodes.NotI32);
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

  console.log("bytes", bytes)

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

  const funcWriter: CodegenFunctionWriter = {
    writer: bytecodeWriter,
    argSlots: 0,
    returnSlots: 0,
    bytecode: [],
    constants: new Map(),
    constantSlots: [],
    nextConstantSlot: 0,
    // localsMap: new Map(),
    locals: [],
    currentScopeIndex: 0,
    nextLocalSlot: 0
  }
  funcWriter.argSlots = Object.values(func.concreteTypes).reduce((acc, x) => acc + slotSize(funcWriter, x), 0); // prettier-ignore
  funcWriter.returnSlots = slotSize(funcWriter, func.returnType);

  func.argBindings.forEach((binding, i) => {
    // funcWriter.localsMap.set(binding, funcWriter.localsMap.size);
    funcWriter.locals.push({ binding, slot: funcWriter.nextLocalSlot, scopeIndex: funcWriter.currentScopeIndex })
    funcWriter.nextLocalSlot += slotSize(funcWriter, binding.type);
  });
  const localSlotsAfterArgs = funcWriter.nextLocalSlot

  writeExpr(funcWriter, func.body);
  writeBytes(funcWriter, OpCodes.Return);

  // Make sure locals/pops match up
  compilerAssert(funcWriter.locals.length === func.argBindings.length, "Compile error got $x expected $y", { x: funcWriter.locals.length, y: func.argBindings.length, fatal: true })
  compilerAssert(funcWriter.nextLocalSlot === localSlotsAfterArgs, "Compile error got $x expected $y", { x: funcWriter.nextLocalSlot, y: localSlotsAfterArgs, fatal: true })

  console.log(funcWriter.writer.typeSizes)

  bytecodeWriter.functions.push(funcWriter);
  return funcWriter
};
