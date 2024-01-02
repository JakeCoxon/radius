import { Ast, AstWriterTable, Binding, BoolType, CodegenFunctionWriter, CodegenWriter, CompiledFunction, DoubleType, ExternalTypeConstructor, FileWriter, FloatType, GlobalCompilerState, IntType, ParameterizedType, Type, VoidType, compilerAssert } from "./defs";

const OpCodes = {
  Nil: 0,
  True: 1,
  False: 2,
  And: 3,
  Or: 4,
  Pop: 5,
  Print: 6,
  StringFormat: 7,
  Jump: 8,
  JumpIfFalse: 9,
  JumpIfFalsePop: 10,
  Loop: 11,
  Call: 12,
  Return: 13,
  ConstantV: 14,
  ConstantF: 15,
  ConstantD: 16,
  ConstantI: 17,
  GetLocalV: 18,
  GetLocalF: 19,
  GetLocalD: 20,
  GetLocalI: 21,
  SetLocalV: 22,
  SetLocalF: 23,
  SetLocalD: 24,
  SetLocalI: 25,
  EqualV: 26,
  EqualF: 27,
  EqualD: 28,
  EqualI: 29,
  GreaterV: 30,
  GreaterF: 31,
  GreaterD: 32,
  GreaterI: 33,
  LessV: 34,
  LessF: 35,
  LessD: 36,
  LessI: 37,
  AddV: 38,
  AddF: 39,
  AddD: 40,
  AddI: 41,
  SubtractV: 42,
  SubtractF: 43,
  SubtractD: 44,
  SubtractI: 45,
  MultiplyV: 46,
  MultiplyF: 47,
  MultiplyD: 48,
  MultiplyI: 49,
  DivideV: 50,
  DivideF: 51,
  DivideD: 52,
  DivideI: 53,
  NotV: 54,
  NotF: 55,
  NotD: 56,
  NotI: 57,
  NegateV: 58,
  NegateF: 59,
  NegateD: 60,
  NegateI: 61,
  ToStringV: 62,
  ToStringF: 63,
  ToStringD: 64,
  ToStringI: 65,
};
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
  // console.log('pushing bytecode')
  writer.bytecode.push(...values);
};
const writeJump = (writer: CodegenFunctionWriter, type: number) => {
  writeBytes(writer, type, 0, 0);
  const jump = writer.bytecode.length;
  return () => writeLittleEndian16At(writer.bytecode, jump - 2, writer.bytecode.length - jump); // prettier-ignore
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
    writeBytes(writer, OpCodes.NotI);
    return;
  } else if (op === "<=") {
    writeOperator(writer, ">", type);
    writeBytes(writer, OpCodes.NotI);
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
  if (type === IntType || type === BoolType) s = 'I'
  else if (type === FloatType) s = 'F' 
  else if (type === DoubleType) s = 'D'
  else compilerAssert(false, "Unsupported type $type", { type })
  compilerAssert((operatorMap as any)[op] !== undefined);
  const bytecode: number | undefined = (OpCodes as any)[`${(operatorMap as any)[op]}${s}`];
  compilerAssert(bytecode !== undefined, "No op found", { op, s });
  writeBytes(writer, bytecode);
};
const slotSize = (writer: CodegenFunctionWriter, type: Type): number => {
  if (type === VoidType) return 0
  if (type === IntType) return 1
  if (type instanceof ParameterizedType) {
    if (writer.writer.typeSizes.get(type)) return writer.writer.typeSizes.get(type)!;
    const s = calculateTypeSize(writer, type);
    writer.writer.typeSizes.set(type, s)
    return s
  }
  compilerAssert(false, "Unexpected type $type", { type });
};
const calculateTypeSize = (writer: CodegenFunctionWriter, type: ParameterizedType) => {
  if (type.typeConstructor instanceof ExternalTypeConstructor) {
    return 1
  }
  return type.typeInfo.fields.reduce((acc, x) => acc + slotSize(writer, x.fieldType), 0)
  // compilerAssert(false, "Not implemented", { t: type.typeConstructor, f: type.typeInfo.fields, x })
}
const writeExpr = (writer: CodegenFunctionWriter, ast: Ast) => {
  compilerAssert(astWriter[ast.key], `Not implemented ast writer '${ast.key}'`)
  astWriter[ast.key](writer, ast as any);
};

const astWriter: AstWriterTable = {
  statements: (writer, ast) => {
    // TODO: Filter voids?
    ast.statements.forEach((expr, i) => {
      writeExpr(writer, expr);
      if (i !== ast.statements.length - 1 && expr.type !== VoidType)
        writeBytes(writer, OpCodes.Pop, slotSize(writer, expr.type));
    })
  },
  string: (writer, ast) => {
    // compilerAssert(false, "Not implemented string")
  },
  binding: (writer, ast) => {
    let index = writer.locals.get(ast.binding);
    compilerAssert(index !== undefined, "Expected binding", { ast, locals: Array.from(writer.locals.values()) });
    compilerAssert(ast.binding.type !== VoidType);
    // if (index === undefined) index = 69; // closure variables
    // compilerAssert(index !== undefined, `Expected local ${ast.values[0].name}`);
    writeBytes(writer, OpCodes.GetLocalI, index);
    // console.log(ast);
  },
  let: (writer, ast) => {
    // console.log('writing binding local')
    // const [binding, type, value] = ast.values;
    writer.locals.set(ast.binding, writer.nextLocalSlot);
    writer.nextLocalSlot += slotSize(writer, ast.binding.type);
    // TODO: No value should zero initialize?
    if (ast.value) writeExpr(writer, ast.value);
  },
  set: (writer, ast) => {
    compilerAssert(ast.binding.type === IntType, `Not implemented type $type`, { type: ast.binding.type });
    const index = writer.locals.get(ast.binding);
    compilerAssert(index !== undefined);
    writeExpr(writer, ast.value);
    writeBytes(writer, OpCodes.SetLocalI, index);
  },
  number: (writer, ast) => {
    let index = writer.constants.get(ast.value);
    if (index === undefined) {
      index = writer.nextConstantSlot;
      writer.nextConstantSlot += slotSize(writer, ast.type);
      writer.constants.set(ast.value, index);
      writeTypeAt(writer.constantSlots, writer.constantSlots.length, ast.type, ast.value);
    }
    writeBytes(writer, OpCodes.ConstantI, index);
  },
  if: (writer, ast) => {
    writeExpr(writer, ast.expr);
    const patch1 = writeJump(writer, OpCodes.JumpIfFalse);
    writeExpr(writer, ast.trueBody);
    if (ast.falseBody) {
      const patch2 = writeJump(writer, OpCodes.Jump);
      patch1();
      writeExpr(writer, ast.falseBody);
      patch2();
    } else patch1()
  },
  while: (writer, ast) => {
    const loop = writer.bytecode.length
    writeExpr(writer, ast.condition)
    const patch1 = writeJump(writer, OpCodes.JumpIfFalse)
    writeExpr(writer, ast.body)
    writeBytes(writer, OpCodes.Jump)
    writeLittleEndian16At(writer.bytecode, loop, loop - writer.bytecode.length)
    patch1()
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
      if (ast.args[0].type === IntType) writeBytes(writer, OpCodes.ToStringI);
      // else compilerAssert(false, `Unsupported ${ast.args[0].type._type}`);
      writeBytes(writer, OpCodes.Print, 1);
      return;
    }
    compilerAssert(false, "Not supported");
    // TODO: func name
    // params.forEach(writeExpr);
    // writeBytes(OpCodes.Call, params.length);
  },
  list: (writer, ast) => {
    ast.args.forEach(x => writeExpr(writer, x))
    // writeBytes(writer, OpCodes.List);
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
    // writeBytes(operatorToBytecode(ast.values[0], ast.values[1].type));
    writeOperator(writer, ast.operator, ast.type);
  },
  not: (writer, ast) => {
    writeExpr(writer, ast.expr);
    writeBytes(writer, OpCodes.NotI);
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
    f.constants.forEach(c => {
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
function writeLittleEndian16(arr: number[], offset: numnber, number: number) {
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
    locals: new Map(),
    nextLocalSlot: 0
  }
  funcWriter.argSlots = Object.values(func.concreteTypes).reduce((acc, x) => acc + slotSize(funcWriter, x), 0); // prettier-ignore
  funcWriter.returnSlots = slotSize(funcWriter, func.returnType);

  func.argBindings.forEach((binding, i) => {
    funcWriter.locals.set(binding, funcWriter.locals.size);
  });

  writeExpr(funcWriter, func.body);
  writeBytes(funcWriter, OpCodes.Return);

  console.log(funcWriter.writer.typeSizes)

  bytecodeWriter.functions.push(funcWriter);
  return funcWriter
};
