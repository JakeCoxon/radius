import { compileClassTask } from "./compiler"
import { Ast, BoolType, ClassDefinition, Closure, CompilerError, ConcreteClassType, DoubleType, EnumVariantAst, ExternalTypeConstructor, FloatLiteralType, FloatType, FunctionDefinition, GlobalCompilerState, IntLiteralType, IntType, NeverType, NumberAst, OperatorAst, ParameterizedType, ParseCall, ParseIdentifier, ParseNode, PrimitiveType, RawPointerType, Scope, ScopeParentSymbol, SourceLocation, StatementsAst, TaskContext, Tuple, Type, TypeCheckConfig, TypeCheckResult, TypeCheckVar, TypeConstructor, TypeField, TypeMatcher, TypeTable, TypeVariable, UnknownObject, VariantCastAst, VoidType, compilerAssert, getUniqueId, isType, u64Type, u8Type } from "./defs"
import { Task, TaskDef } from "./tasks"

export const isTypeInteger = (type: Type) => type === IntType || type === u64Type || type === u8Type
export const isTypeFloating = (type: Type) => type === FloatType || type === DoubleType
export const isTypeScalar = (type: Type) => isTypeInteger(type) || isTypeFloating(type)


export const isTypeOption = (type: Type): type is ParameterizedType => {
  return type instanceof ParameterizedType && type.typeConstructor === OptionTypeConstructor
}

export const getCommonType = (types: Type[]): Type => {
  const types2 = types.filter(x => x !== NeverType)
  if (types2.length === 0) return NeverType
  if (types2.length === 1) return types2[0]
  if (types2.length !== types.length) return getCommonType(types2)

  // Special case for Option for now
  if (isTypeOption(types[0])) {
    compilerAssert(types.every(x => isTypeOption(x)), "Expected all types to be option")
    const typesP = types as ParameterizedType[]
    if (typesP.every(x => x.args[0] === typesP[0].args[0])) return typesP[0]
    const opt = typesP.find(x => x.args[0] !== NeverType)!
    compilerAssert(typesP.every(x => canAssignUnknownTo(x.args[0], opt.args[0])), "Expected all types to be the assignable")
    return typesP[0]
  }
  
  if (types.some(x => x === FloatLiteralType || x === FloatType)) {
    compilerAssert(types.every(x => x === IntLiteralType || x === FloatLiteralType || x === FloatType), "Expected types to be the same", { types })
    return FloatType
  }
  if (types.some(x => x === IntLiteralType || x === IntType)) {
    compilerAssert(types.every(x => x === IntLiteralType || x === IntType), "Expected types to be the same", { types })
    return IntType
  }
  
  compilerAssert(types.every(x => x === types[0]), "Expected types to be the same", { types })
  return types2[0];
}

const typeTableGet = (typeTable: TypeTable, type: Type) => {
  for (const t of typeTable.array) {
    if (typesEqual(t, type)) return t;
  }
}

const typeTableInsert = (typeTable: TypeTable, type: Type) => {
  typeTable.array.push(type);
  return type;
}

export const typeTableGetOrInsert = (typeTable: TypeTable, type: Type) => {
  let v = typeTableGet(typeTable, type)
  if (v) return v;
  return typeTableInsert(typeTable, type)
}

export const hashValues = (values: unknown[], info={}) => {
  return values.map(value => {
    if (typeof value === 'number') return value
    if (value instanceof PrimitiveType) return `$${value.typeName}`
    if (value instanceof ParameterizedType) return getUniqueId(value)
    if (value instanceof ConcreteClassType) return getUniqueId(value)
    if (value instanceof ClassDefinition) return getUniqueId(value)
    if (value instanceof FunctionDefinition) return getUniqueId(value)
    if (value instanceof Closure) return getUniqueId(value)
    compilerAssert(false, "Cannot hash value", { value, ...info })
  }).join("__")
}

// Don't use directly, use type table to see if types are equal
export const typesEqual = (t1: unknown, t2: any): boolean => {
  if (Object.getPrototypeOf(t1) !== Object.getPrototypeOf(t2)) return false;
  if (t1 instanceof ExternalTypeConstructor) return t1 === t2;
  if (!isType(t1)) {
    return hashValues([t1]) === hashValues([t2])
  }
  compilerAssert(t1 && t2, "Unexpected", { t1, t2 })
  if (t1 instanceof PrimitiveType) return t1 == t2;
  if (t1 instanceof ConcreteClassType) return t1.compiledClass == t2.compiledClass;
  if (t1 instanceof ParameterizedType) {
    if (!typesEqual(t1.typeConstructor, t2.typeConstructor)) return false;
    if (t1.args.length !== t2.args.length) return false;
    return t1.args.every((x, i) => typesEqual(x, t2.args[i]))
  }
  return false;
}

export const typeMatcherEquals = (result: TypeCheckResult, matcher: TypeMatcher, expected: Type, substitutions: UnknownObject) => {
  const testTypeConstructor = (matcher: ExternalTypeConstructor | ClassDefinition, expected: TypeConstructor) => {
    if (matcher instanceof ExternalTypeConstructor) {
      if (matcher === expected) return true
      compilerAssert(false, "$matcher does not equal $expected", { matcher, expected })
    }
    if (expected instanceof ClassDefinition) {
      if (matcher !== expected) {
        compilerAssert(false, "$matcher does not equal $expected", { matcher, expected: expected })
        return false;
      }
      return true
    }
    compilerAssert(false, "Not implemented", { matcher, expected })
  }
  
  const test = (matcher: unknown, expected: unknown) => {
    if (matcher instanceof TypeMatcher && expected instanceof ParameterizedType) {
      if (!testTypeConstructor(matcher.typeConstructor, expected.typeConstructor)) {
        compilerAssert(false, "Not implemented", { matcher, expected })
        return false;
      }

      let i = 0;
      for (const arg of matcher.args) {
        if (!test(arg, expected.args[i])) return false;
        i++
      }
      return true;
    }
    if (matcher instanceof TypeVariable) {
      if (substitutions[matcher.name]) return substitutions[matcher.name] === expected;
      compilerAssert(!(expected instanceof ClassDefinition), "Programmer error. Types must have been already compiled by this point", { matcher, expected, func: result.func })
      substitutions[matcher.name] = expected;
      return true
    }
    if (matcher instanceof TypeMatcher && expected instanceof ConcreteClassType) {
      compilerAssert(false, "Not implemented", { matcher, expected })
      return false;
    }
    compilerAssert(false, "Not implemented", { matcher, expected })
  }
  return test(matcher, expected)
}

// Adds a special typeCheckResult field and sets checkFailed so we see what exactly failed
export function typeCheckAssert(result: TypeCheckResult, condition: boolean, message: string, info: object = {}): asserts condition { 
  if (!condition) result.checkFailed = true
  compilerAssert(condition, message, {...info })
}

export const expandMatcher = (result: TypeCheckResult, matcher: Type | TypeMatcher | TypeVariable): Type => {
  if (isType(matcher)) return matcher
  if (matcher instanceof TypeVariable) {
    let type = result.substitutions[matcher.name]
    compilerAssert(type, "Type variable $name not found", { name: matcher.name, substitutions: result.substitutions })
    compilerAssert(isType(type), "Expected type got $type", { type, substitutions: result.substitutions })
    return type
  }
  compilerAssert(false, "Not implemented yet", { matcher })
}

export const isTypeOrMatcher = (type: unknown): type is Type | TypeMatcher | TypeVariable => {
  return isType(type) || type instanceof TypeMatcher || type instanceof TypeVariable
}

export const typeCheckFunctionResult = (result: TypeCheckResult, compiledParamTypes: unknown[], returnType: unknown) => {

  compiledParamTypes.forEach((paramType, i) => {
    const givenArg = result.sortedArgs[i]
    const fromType: TypeCheckVar = { type: givenArg.type }
    if (isType(paramType)) normalizeNumberType(fromType, { type: paramType })
    numberTypeToConcrete(fromType)

    if (paramType === null) {
    } else if (paramType instanceof TypeMatcher) {
      const matches = typeMatcherEquals(result, paramType, fromType.type, result.substitutions)
      typeCheckAssert(result, matches, "Type check failed. Expected $expected got $got", { expected: paramType, got: fromType.type })
    } else if (paramType instanceof TypeVariable) {
      if (result.substitutions[paramType.name]) {
        compilerAssert(result.substitutions[paramType.name] === fromType.type, "Tried to substitute $name with $got but but it was already substituted with $existing", { name: paramType.name, existing: result.substitutions[paramType.name], got: fromType.type })
      }
      result.substitutions[paramType.name] = fromType.type
    } else {
      compilerAssert(isType(paramType), "Expected type got $paramType", { paramType });
      const param = result.func.params[i]
      typeCheckAssert(result, fromType.type === paramType, "Argument $name of type $value does not match $expected", { name: param.name.token, value: fromType.type, expected: paramType })
    }

    compilerAssert(fromType.type !== IntLiteralType && fromType.type !== FloatLiteralType, "Unexpected literal type", { type: result.concreteTypes.at(-1) })

    result.concreteTypes.push(fromType.type)
  })

  compilerAssert(isTypeOrMatcher(returnType), "Expected type got $returnType", { returnType })
  result.returnType = expandMatcher(result, returnType)

}

export const isParameterizedTypeOf = (a: Type, expected: TypeConstructor): a is ParameterizedType => {
  return a instanceof ParameterizedType && a.typeConstructor === expected;
}

// This is a bit messy but we want Array!Foo to compile Foo into a concrete type
// in various cases (class type arguments, function type arguments, etc)
export const classDefinitionToType = (classDef: ClassDefinition): Task<ConcreteClassType, CompilerError> => {
  if (classDef.concreteType) return Task.of(classDef.concreteType)
  compilerAssert(classDef.typeArgs.length === 0, "Class $classDef expected $expected type arguments but got none", { classDef, expected: classDef.typeArgs.length })
  return (
    TaskDef(compileClassTask, { classDef, typeArgs: [] })
    .chainFn((task, res) => { compilerAssert(res instanceof ConcreteClassType); return Task.of(res) })
  );
}

export const typeArgumentsToType = (typeArgs: unknown[]) => {
  return Task.concurrency(typeArgs.map(typeArg => {
    if (typeArg instanceof ClassDefinition) 
      return classDefinitionToType(typeArg)
    return Task.of(typeArg)
  }))
}


export const compileTypeConstructorTask = (ctx: TaskContext, classDef: ClassDefinition, typeArgs: unknown[]): Task<Type | TypeMatcher, CompilerError> => {
  compilerAssert(classDef.isTypeConstructor, "Expected type constructor class got $func", { classDef });

  // Resolve ClassDefinitions into concrete types before instantiating any classes or functions
  // This is only 1 level deep because the assumption is that the deeper levels have already
  // been resolved by the time we get here 
  if (typeArgs.some(x => x instanceof ClassDefinition)) {
    return (
      typeArgumentsToType(typeArgs).chainFn((task, compiledTypeArgs) => 
        TaskDef(compileTypeConstructorTask, classDef, compiledTypeArgs) // Call self
      )
    )
  }
  
  const typeVars = typeArgs.filter((x): x is TypeVariable => x instanceof TypeVariable)
  if (typeVars.length) {
    return Task.of(new TypeMatcher(classDef, typeArgs, typeVars))
  }
  return TaskDef(compileClassTask, { classDef, typeArgs })
}

export const createParameterizedExternalType = (globalCompiler: GlobalCompilerState, typeConstructor: ExternalTypeConstructor, argTypes: unknown[]): Task<Type, CompilerError> => {
  const newArgTypes = argTypes.map(value => {
    // TODO: Do this more rigorously?
    if (isType(value)) {
      if (value === IntLiteralType) return IntType
      if (value === FloatLiteralType) return FloatType
      return value;
    }
    if (value instanceof ClassDefinition && value.concreteType) return value.concreteType
    if (value instanceof Tuple) compilerAssert(false, "Not implemented yet")
    compilerAssert(false, "Expected types got $expected", { value }); 
  })
  return (
    typeConstructor.createType(globalCompiler, newArgTypes)
    .chainFn((task, type) => {
      return Task.of(typeTableGetOrInsert(globalCompiler.typeTable, type))
    })
  )
}

export const normalizeNumberType = (from: TypeCheckVar, to: TypeCheckVar) => {
  if (from.type === IntLiteralType) {
    if (to.type === FloatType) from.type = FloatType
    else if (to.type === u8Type) from.type = u8Type
    else if (to.type === u64Type) from.type = u64Type
    else if (to.type === DoubleType) from.type = DoubleType
    else if (to.type === FloatLiteralType) from.type = FloatLiteralType
    else if (to.type === IntType) from.type = IntType
  }
  else if (from.type === FloatLiteralType) {
    if (to.type === FloatType) from.type = FloatType
    else if (to.type === DoubleType) from.type = DoubleType
  }
}
export const numberTypeToConcrete = (from: TypeCheckVar) => {
  if (from.type === IntLiteralType) from.type = IntType
  else if (from.type === FloatLiteralType) from.type = FloatType
}

export const canAssignUnknownTo = (from: unknown, to: unknown) => {
  if (from === to) return true
  return isType(from) && isType(to) && canAssignTypeTo(from, to)
}
export const canAssignTypeTo = (from: Type, to: Type) => {
  if (from === to) return true
  const a = { type: from }, b = { type: to }
  normalizeNumberType(a, b)
  if (a.type === b.type) return true
  
  if (a.type === NeverType) return true

  if (a.type instanceof ParameterizedType && b.type instanceof ParameterizedType) {
    if (a.type.typeConstructor !== b.type.typeConstructor) return false
    if (a.type.args.length !== b.type.args.length) return false
    for (let i = 0; i < a.type.args.length; i++) {
      if (!canAssignUnknownTo(a.type.args[i], b.type.args[i])) return false
    }
    return true
  }

  return false
}

export const typecheckNumberOperator = (config: TypeCheckConfig): void => {
  normalizeNumberType(config.a, config.b)
  normalizeNumberType(config.b, config.a)
  compilerAssert(config.a.type === config.b.type, "Expected int, float or double type got $a $b", { a: config.a.type, b: config.b.type })
  config.inferType = config.b.type
}

export const typecheckNumberComparison = (config: TypeCheckConfig) => {
  normalizeNumberType(config.a, config.b)
  normalizeNumberType(config.b, config.a)
  numberTypeToConcrete(config.a)
  numberTypeToConcrete(config.b)
  const aok = isTypeScalar(config.a.type)
  const bok = isTypeScalar(config.b.type)
  compilerAssert(aok && bok, "Expected int, float or double type got $a $b", { a: config.a.type, b: config.b.type })
  config.inferType = BoolType
}

export const typecheckEquality = (config: TypeCheckConfig) => {
  normalizeNumberType(config.a, config.b)
  normalizeNumberType(config.b, config.a)
  numberTypeToConcrete(config.a)
  numberTypeToConcrete(config.b)
  const aok = config.a.type === BoolType || isTypeScalar(config.a.type) || config.a.type === RawPointerType
  const bok = config.b.type === BoolType || isTypeScalar(config.b.type) || config.b.type === RawPointerType
  compilerAssert(aok && bok, "Expected bool, int, float or double, rawptr type got $a $b", { a: config.a.type, b: config.b.type })
  config.inferType = BoolType
}

export const propagateLiteralType = (inferType: Type, ast: Ast | null): Type => {
  // This is for literals only, not for types that need implicit casts
  if (!ast) return inferType
  if (inferType === IntLiteralType) inferType = IntType
  if (inferType === FloatLiteralType) inferType = FloatType
  if (inferType === ast.type) return inferType
  if (!canAssignTypeTo(ast.type, inferType)) return inferType

  const recur = (ast: Ast) => {
    if (inferType === ast.type) return
    if (ast instanceof VariantCastAst) {
      // This is to support None!never into Option!int, assume that the type was originally allowed to contain never
      ast.type = inferType;
    } else if (ast instanceof EnumVariantAst) {
      ast.type = inferType;
      ast.enumType = inferType
      const variantType = ast.variantType
      compilerAssert(variantType instanceof ParameterizedType, "Expected parameterized type", { ast });
      ast.variantType = (inferType.typeInfo.metaobject.variants as Type[]).find(x => x instanceof ParameterizedType && x.typeConstructor === variantType.typeConstructor)!
      compilerAssert(ast.variantType, "Expected variant type", { ast, variantType })
    } else if (ast instanceof NumberAst) {
      ast.type = inferType
    } else if (ast instanceof OperatorAst) {
      ast.type = inferType;
      recur(ast.args[0]); recur(ast.args[1])
    } else if (ast instanceof StatementsAst) {
      ast.type = inferType;
      recur(ast.statements.at(-1)!)
    }
  }
  recur(ast)
  return inferType
}
export const propagatedLiteralAst = (ast: Ast) => { propagateLiteralType(ast.type, ast); return ast }

export const calculateSizeOfType =  (expr: Type): number => {
  // TODO: Padding

  if (expr instanceof ConcreteClassType) {
    compilerAssert(expr, "Expected concrete type") // TODO: Compile it?
    return expr.typeInfo.fields.reduce((acc, x) => acc + calculateSizeOfType(x.fieldType), 0)
  }
  if (expr instanceof PrimitiveType) { return expr.typeInfo.sizeof }
  if (expr instanceof ParameterizedType) { 
    return expr.typeInfo.fields.reduce((acc, x) => acc + calculateSizeOfType(x.fieldType), 0)
  }
  compilerAssert(false, "Not implemented", { expr })
}


export const NoneTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("None", (compiler, argTypes) => {
  const argType = argTypes[0] || NeverType
  compilerAssert(isType(argType), "Expected one type arg", { argType })
  const sizeof = argType.typeInfo.sizeof + IntType.typeInfo.sizeof
  const variantPadding = argType.typeInfo.sizeof
  const type = new ParameterizedType(NoneTypeConstructor, [argType], { sizeof, variantPadding, fields: [], metaobject: Object.create(null), isReferenceType: false });
  type.typeInfo.isInvalidSize = argType === NeverType
  type.typeInfo.metaobject.isEnumVariant = true
  type.typeInfo.metaobject.enumConstructorVariantOf = OptionTypeConstructor
  type.typeInfo.metaobject.enumVariantIndex = 0
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "tag", type, 0, IntType))
  return Task.of(type)
})

export const SomeTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("Some", (compiler, argTypes) => {
  compilerAssert(argTypes.length === 1, "Expected one type arg", { argTypes })
  const argType = argTypes[0]
  const sizeof = argType.typeInfo.sizeof + IntType.typeInfo.sizeof
  const type = new ParameterizedType(SomeTypeConstructor, [argType], { sizeof, fields: [], metaobject: Object.create(null), isReferenceType: false });
  type.typeInfo.metaobject.isEnumVariant = true
  type.typeInfo.metaobject.enumConstructorVariantOf = OptionTypeConstructor
  type.typeInfo.metaobject.enumVariantIndex = 1
  type.typeInfo.isInvalidSize = argType === NeverType
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "tag", type, 0, IntType))
  type.typeInfo.fields.push(new TypeField(SourceLocation.anon, "value", type, 1, argType))
  return Task.of(type)
})

export const OptionTypeConstructor: ExternalTypeConstructor = new ExternalTypeConstructor("Option", (compiler, argTypes) => {
  const argType = argTypes[0] || NeverType
  compilerAssert(isType(argType), "Expected one type arg", { argType })

  const sizeof = argType.typeInfo.sizeof + IntType.typeInfo.sizeof
  const variantPadding = argType.typeInfo.sizeof
  const opttype = new ParameterizedType(OptionTypeConstructor, [argType], { sizeof, variantPadding, fields: [], metaobject: Object.create(null), isReferenceType: false });

  opttype.typeInfo.isInvalidSize = argType === NeverType
  opttype.typeInfo.metaobject.isEnum = true
  opttype.typeInfo.fields.push(new TypeField(SourceLocation.anon, "tag", opttype, 0, IntType))

  return (
    createParameterizedExternalType(compiler, SomeTypeConstructor, [argType])
    .chainFn((task, someType) => {
      return (
        createParameterizedExternalType(compiler, NoneTypeConstructor, [argType])
        .chainFn((task, noneType) => {
          opttype.typeInfo.metaobject.variants = [someType, noneType]
          opttype.typeInfo.metaobject.Some = someType
          opttype.typeInfo.metaobject.None = noneType
          return Task.of(opttype)
        })
      )
    })
  )
})