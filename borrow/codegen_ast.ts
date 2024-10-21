import { externalBuiltinBindings } from "../src/compiler_sugar";
import { Binding, BindingAst, CallAst, Capability, CompiledClass, CompiledFunction, ConcreteClassType, FieldAst, FunctionParameter, PrimitiveType, RawPointerType, SetFieldAst, SourceLocation, StatementsAst, Type, TypeField, TypeInfo, UserCallAst, VoidType } from "../src/defs";
import { compilerAssert } from "./defs";

export const generateConstructor = (structName: string, structType: Type) => {
  const funcParams: FunctionParameter[] = [];
  const argBindings: Binding[] = [];
  const concreteTypes: Type[] = [];

  const setArgBinding = new Binding('param', structType);
  argBindings.push(setArgBinding);
  funcParams.push(new FunctionParameter(setArgBinding, structType, true, RawPointerType, Capability.Set));
  concreteTypes.push(structType);

  const fields = structType.typeInfo.fields;
  fields.forEach((f, i) => {
    const type = f.fieldType;
    fields[i] = new TypeField(SourceLocation.anon, f.name, structType, i, type);
    const argBinding = new Binding(f.name, type);
    funcParams.push(createParameter(argBinding, Capability.Sink));
    argBindings.push(argBinding);
    concreteTypes.push(type);
  });
  const constructorBinding = new Binding(`constructor${structName}`, VoidType);

  const [param, ...fieldBindings] = argBindings;
  const fieldAsts = fields.map((field, i) => {
    const valueBinding = new BindingAst(field.fieldType, SourceLocation.anon, fieldBindings[i]);
    const struct = new BindingAst(structType, SourceLocation.anon, param);
    return new SetFieldAst(VoidType, SourceLocation.anon, struct, field, valueBinding);
  });
  const constructorBody = new StatementsAst(VoidType, SourceLocation.anon, fieldAsts);

  const compiledFunc = new CompiledFunction(constructorBinding, { debugName: constructorBinding.name } as any, VoidType, concreteTypes, constructorBody, argBindings, funcParams, [], 0);
  return compiledFunc
}

export const generateDestructor = (structName: string, structType: Type) => {
  const funcParams: FunctionParameter[] = [];
  const argBindings: Binding[] = [];
  const concreteTypes: Type[] = [];

  const setArgBinding = new Binding('param', structType);
  argBindings.push(setArgBinding);
  funcParams.push(new FunctionParameter(setArgBinding, structType, true, RawPointerType, Capability.Sink));
  concreteTypes.push(structType);

  // An empty function is enough because later passes will insert the
  // necessary instructions sink instructions
  const destructorBinding = new Binding(`destructor${structName}`, VoidType);
  const destructorBody = new StatementsAst(VoidType, SourceLocation.anon, []);
  const compiledFunc = new CompiledFunction(destructorBinding, { debugName: destructorBinding.name } as any, VoidType, concreteTypes, destructorBody, argBindings, funcParams, [], 0);
  compiledFunc.isDestructor = true;
  return compiledFunc
}

export const generateMoveFunction = (structType: Type, fnName: string, destCapability: Capability, sourceCapability: Capability) => {
  const funcParams: FunctionParameter[] = [];
  const argBindings: Binding[] = [];
  const concreteTypes: Type[] = [];

  const setArgBinding = new Binding('dst', structType);
  argBindings.push(setArgBinding);
  funcParams.push(new FunctionParameter(setArgBinding, structType, true, RawPointerType, destCapability));
  concreteTypes.push(structType);

  const srcArgBinding = new Binding('src', structType);
  argBindings.push(srcArgBinding);
  funcParams.push(new FunctionParameter(srcArgBinding, structType, true, RawPointerType, sourceCapability));
  concreteTypes.push(structType);

  const binding = new Binding(fnName, VoidType);
  const constructorBinding = structType.typeInfo.metaobject.constructorBinding
  compilerAssert(constructorBinding && constructorBinding instanceof Binding, `Constructor not found for ${structType.shortName}`);

  const passFieldAsts = structType.typeInfo.fields.map((f, i) => {
    const arg = new BindingAst(structType, SourceLocation.anon, srcArgBinding);
    const field = new FieldAst(f.fieldType, SourceLocation.anon, arg, f);
    const value = (() => {
      if (sourceCapability === Capability.Sink) return field;
      compilerAssert(sourceCapability === Capability.Let, `Invalid source capability ${sourceCapability}`);
      // Let capability means copy each field
      return new UserCallAst(f.fieldType, SourceLocation.anon, externalBuiltinBindings.copy, [field]);
    })()
    return new SetFieldAst(VoidType, SourceLocation.anon, new BindingAst(structType, SourceLocation.anon, setArgBinding), f, value);
  });

  const body = new StatementsAst(VoidType, SourceLocation.anon, passFieldAsts);
  return new CompiledFunction(binding, { debugName: fnName } as any, VoidType, concreteTypes, body, argBindings, funcParams, [], 0);
}

export const createParameter = (binding: Binding, capability: Capability) => {
  // @ParameterPassing
  const reference = !((capability === Capability.Let || capability === Capability.Sink)
    && binding.type instanceof PrimitiveType);
  const passingType = reference ? RawPointerType : binding.type;
  return new FunctionParameter(binding, binding.type, reference, passingType, capability);
}