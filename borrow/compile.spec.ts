import { describe, it, expect } from "bun:test"
import { NumberAst, SourceLocation, IntType, Binding, Ast, VoidAst, VoidType, LetAst, isType, OperatorAst, SetAst, BindingAst, StatementsAst, TypeInfo, TypeField, ConcreteClassType, CompiledClass, ConstructorAst, FieldAst, SetFieldAst, FunctionDefinition, CompiledFunction, CallAst, ReturnAst, IfAst, BoolType, AndAst, WhileAst, FunctionParameter, Capability, RawPointerType, Type, PrimitiveType } from "../src/defs";
import { ASTNode, AndNode, AssignmentNode, BinaryExpressionNode, BlockStatementNode, CallExpressionNode, CreateStructNode, ExpressionStatementNode, FunctionDeclarationNode, FunctionParameterNode, IdentifierNode, IfStatementNode, LetConstNode, LiteralNode, MemberExpressionNode, Module, PrintNode, ProgramNode, ReturnNode, VariableDeclarationNode, WhileStatementNode, compilerAssert, printIR, printLivenessMap, textColors } from "./defs";
import { CodeGenerator } from "./codegen";
import { externalBuiltinBindings } from "../src/compiler_sugar";

class Scope {
  constants: Record<string, any> = {}
  variables: Record<string, Binding> = {}
  functions: Record<string, CompiledFunction> = {}
}

export class BasicCompiler {

  scopeStack: Scope[] = [new Scope()]
  rootScope: Scope = this.scopeStack[0]
  scope: Scope = this.rootScope

  allFunctions: Map<Binding, CompiledFunction> = new Map()
  
  constructor(public codegen: CodeGenerator) {
    this.defineConstant('int', IntType)
    this.defineConstant('bool', BoolType)
    this.defineConstant('void', VoidType)
    this.defineConstant('ptr', RawPointerType)
    this.defineType('Point', [
      { name: 'x', type: 'int' },
      { name: 'y', type: 'int' },
    ])
    this.defineType('Line', [
      { name: 'p1', type: 'Point' },
      { name: 'p2', type: 'Point' },
    ])
  }

  defineConstant(name: string, value: any) {
    this.scope.constants[name] = value
  }

  getConstant(name: string) {
    let i = this.scopeStack.length - 1
    while (i >= 0) {
      if (this.scopeStack[i].constants[name]) { return this.scopeStack[i].constants[name] }
      i--
    }
  }

  getVariable(name: string) {
    let i = this.scopeStack.length - 1
    while (i >= 0) {
      if (this.scopeStack[i].variables[name]) { return this.scopeStack[i].variables[name] }
      i--
    }
  }

  getFunction(name: string) {
    let i = this.scopeStack.length - 1
    while (i >= 0) {
      if (this.scopeStack[i].functions[name]) { return this.scopeStack[i].functions[name] }
      i--
    }
  }

  defineType(name: string, fields: { name: string, type: string }[]) {
    const typeFields: TypeField[] = []
    const typeInfo: TypeInfo = { sizeof: 0, fields: typeFields, metaobject: {}, isReferenceType: false }
    const binding = new Binding(name, VoidType)
    const compiledClass = new CompiledClass(SourceLocation.anon, name, binding, null!, null!, null!, typeFields, [], 0)
    let structType = new ConcreteClassType(compiledClass, typeInfo)

    fields.forEach((f, i) => {
      const type = this.getType(f.type)
      typeFields[i] = new TypeField(SourceLocation.anon, f.name, structType, i, type)
    })

    this.defineConstant(name, structType)
    this.generateConstructor(name, structType)
    const moveInitBinding = this.generateMoveFunction(structType, `moveInit${name}`, Capability.Set, Capability.Sink)
    const moveAssignBinding = this.generateMoveFunction(structType, `moveAssign${name}`, Capability.Inout, Capability.Sink)
    const copyConstructorBinding = this.generateMoveFunction(structType, `copy${name}`, Capability.Set, Capability.Let)
    Object.assign(typeInfo.metaobject, { moveInitBinding, moveAssignBinding, copyConstructorBinding })
    
    return structType
  }

  generateConstructor(structName: string, structType: Type) {
    const funcParams: FunctionParameter[] = []
    const argBindings: Binding[] = []
    const concreteTypes: Type[] = []

    const setArgBinding = new Binding('param', structType)
    argBindings.push(setArgBinding)
    funcParams.push(new FunctionParameter(setArgBinding, structType, true, RawPointerType, Capability.Set))
    concreteTypes.push(structType)
    
    const fields = structType.typeInfo.fields
    fields.forEach((f, i) => {
      const type = f.fieldType
      fields[i] = new TypeField(SourceLocation.anon, f.name, structType, i, type)
      const argBinding = new Binding(f.name, type)
      funcParams.push(this.createParameter(argBinding, Capability.Sink))
      argBindings.push(argBinding)
      concreteTypes.push(type)
    })
    const constructorBinding = structType.typeInfo.metaobject.constructorBinding = new Binding(`constructor${structName}`, VoidType)
    
    const [param, ...fieldBindings] = argBindings
    const fieldAsts = fields.map((field, i) => {
      const valueBinding = new BindingAst(field.fieldType, SourceLocation.anon, fieldBindings[i])
      const struct = new BindingAst(structType, SourceLocation.anon, param);
      return new SetFieldAst(VoidType, SourceLocation.anon, struct, field, valueBinding)
    })
    const constructorBody = new StatementsAst(VoidType, SourceLocation.anon, fieldAsts)

    const compiledFunc = new CompiledFunction(constructorBinding, {} as any, VoidType, concreteTypes, constructorBody, argBindings, funcParams, [], 0)
    this.allFunctions.set(constructorBinding, compiledFunc)
  }

  generateMoveFunction(structType: Type, fnName: string, destCapability: Capability, sourceCapability: Capability) {
    const funcParams: FunctionParameter[] = []
    const argBindings: Binding[] = []
    const concreteTypes: Type[] = []

    const setArgBinding = new Binding('dst', structType)
    argBindings.push(setArgBinding)
    funcParams.push(new FunctionParameter(setArgBinding, structType, true, RawPointerType, destCapability))
    concreteTypes.push(structType)
    
    const srcArgBinding = new Binding('src', structType)
    argBindings.push(srcArgBinding)
    funcParams.push(new FunctionParameter(srcArgBinding, structType, true, RawPointerType, sourceCapability))
    concreteTypes.push(structType)

    const binding = new Binding(fnName, VoidType)
    const constructorBinding = structType.typeInfo.metaobject.constructorBinding as Binding

    const passFieldAsts = structType.typeInfo.fields.map((f, i) => {
      const arg = new BindingAst(structType, SourceLocation.anon, srcArgBinding);
      const field = new FieldAst(f.fieldType, SourceLocation.anon, arg, f)
      if (sourceCapability === Capability.Sink) return field
      return new CallAst(f.fieldType, SourceLocation.anon, externalBuiltinBindings.copy, [field], [])
    })
    const call = new CallAst(structType, SourceLocation.anon, constructorBinding, [new BindingAst(setArgBinding.type, SourceLocation.anon, setArgBinding), ...passFieldAsts], [])
    const body = new StatementsAst(VoidType, SourceLocation.anon, [call])

    const compiledFunc = new CompiledFunction(binding, {} as any, VoidType, concreteTypes, body, argBindings, funcParams, [], 0)
    this.allFunctions.set(binding, compiledFunc)
    return binding
  }

  createParameter(binding: Binding, capability: Capability) {
    // @ParameterPassing
    const reference = !((capability === Capability.Let || capability === Capability.Sink)
      && binding.type instanceof PrimitiveType)
    const passingType = reference ? RawPointerType : binding.type
    return new FunctionParameter(binding, binding.type, reference, passingType, capability)
  }

  compile(node: ASTNode): Ast {

    if (node instanceof ProgramNode) {
      const body = new BlockStatementNode(node.body)
      return this.compile(new FunctionDeclarationNode('main', [], 'void', body))
    }

    if (node instanceof BlockStatementNode) {
      const statements = node.body.map(x => this.compile(x))
      return new StatementsAst(VoidType, SourceLocation.anon, statements)
    }

    if (node instanceof LetConstNode) {
      this.defineConstant(node.name, node.value)
      return new VoidAst(VoidType, SourceLocation.anon)
    }
    
    if (node instanceof VariableDeclarationNode) {
      const type = this.getType(node.type)
      const binding = new Binding(node.name, type)
      this.scope.variables[node.name] = binding
      const value = node.initializer ? this.compile(node.initializer) : null
      return new LetAst(VoidType, SourceLocation.anon, binding, value, node.mutable)
    }
    
    if (node instanceof LiteralNode) {
      if (typeof node.value === 'number') {
        return new NumberAst(IntType, SourceLocation.anon, node.value)
      }
      compilerAssert(false, 'Not implemented LiteralNode', { ast: node })
    }
    
    if (node instanceof ExpressionStatementNode) {
      const expression = this.compile(node.expression)
      return expression
    }
    
    if (node instanceof BinaryExpressionNode) {
      const left = this.compile(node.left)
      const right = this.compile(node.right)
      compilerAssert(left.type === right.type, 'Type mismatch', { left, right })
      return new OperatorAst(left.type, SourceLocation.anon, node.operator, [left, right])
    }
    
    if (node instanceof AssignmentNode) {
      const left = this.compile(node.left)
      const right = this.compile(node.right)
      compilerAssert(left.type === right.type, 'Type mismatch', { left, right, leftType: left.type, rightType: right.type })
      if (left instanceof BindingAst) {
        return new SetAst(left.type, SourceLocation.anon, left.binding, right)
      }
      if (left instanceof FieldAst) {
        return new SetFieldAst(VoidType, SourceLocation.anon, left.left, left.field, right)
      }
      compilerAssert(false, 'Not implemented AssignmentNode', { left, right })
    }

    if (node instanceof IdentifierNode) {
      const binding = this.getVariable(node.name)
      compilerAssert(binding, 'Variable not found', { ast: node })
      return new BindingAst(binding.type, SourceLocation.anon, binding)
    }

    if (node instanceof CreateStructNode) {
      const type = this.getType(node.name)
      const fields = node.fields.map(x => this.compile(x))
      // const binding = type.typeInfo.metaobject.constructorBinding as Binding
      // const call = new CallAst(type, SourceLocation.anon, binding, fields, [])
      // const letBinding = new Binding('temp', type)
      // const let_ = new LetAst(type, SourceLocation.anon, letBinding, null, true)
      // return new StatementsAst(type, SourceLocation.anon, [let_, call])
      return new ConstructorAst(type, SourceLocation.anon, fields)
    }

    if (node instanceof MemberExpressionNode) {
      const object = this.compile(node.object)
      const field = node.property
      const type = object.type
      const fieldIndex = type.typeInfo.fields.findIndex(x => x.name === field)
      compilerAssert(fieldIndex >= 0, 'Field not found', { object, field, type })
      return new FieldAst(type.typeInfo.fields[fieldIndex].fieldType, SourceLocation.anon, object, type.typeInfo.fields[fieldIndex])
    }

    if (node instanceof FunctionDeclarationNode) {
      compilerAssert(!this.getFunction(node.name), 'Function already defined', { ast: node })
      this.scopeStack.push(new Scope())
      this.scope = this.scopeStack[this.scopeStack.length - 1]

      const concreteTypes = node.params.map(x => this.getType(x.type))
      const argBindings = node.params.map((x, i) => new Binding(x.name, concreteTypes[i]))

      argBindings.forEach((param, i) => {
        this.scope.variables[param.name] = param
      })
      const body = this.compile(node.body)

      this.scopeStack.pop()
      this.scope = this.scopeStack[this.scopeStack.length - 1]

      const binding = new Binding(node.name, VoidType)
      const returnType = this.getType(node.returnType)
      const funcDef: FunctionDefinition = { debugName: node.name } as any // Just need debugName for now

      const parameters = argBindings.map((x, i) => {
        return this.createParameter(x, node.params[i].capability)
      })

      const compiledFunc = new CompiledFunction(binding, funcDef, returnType, concreteTypes, body, argBindings, parameters, [], 0)
      this.scope.functions[node.name] = compiledFunc
      this.allFunctions.set(binding, compiledFunc)

      return new VoidAst(VoidType, SourceLocation.anon)
    }

    if (node instanceof ReturnNode) {
      const value = node.argument ? this.compile(node.argument) : null
      return new ReturnAst(VoidType, SourceLocation.anon, value)
    }

    if (node instanceof CallExpressionNode) {
      const func = this.getFunction(node.callee)
      compilerAssert(func, 'Function not found', { ast: node })
      const args = node.args.map(x => this.compile(x))
      const binding = func.binding
      return new CallAst(func.returnType, SourceLocation.anon, binding, args, [])
    }

    if (node instanceof PrintNode) {
      const value = this.compile(node.value)
      if (value.type === IntType) return new CallAst(VoidType, SourceLocation.anon, externalBuiltinBindings.printInt, [value], [])
      return new CallAst(VoidType, SourceLocation.anon, externalBuiltinBindings.print, [value], [])
    }

    if (node instanceof IfStatementNode) {
      const test = this.compile(node.condition)
      const consequent = this.compile(node.consequent)
      const alternate = node.alternate ? this.compile(node.alternate) : null
      return new IfAst(VoidType, SourceLocation.anon, test, consequent, alternate)
    }

    if (node instanceof AndNode) {
      const left = this.compile(node.left)
      const right = this.compile(node.right)
      return new AndAst(BoolType, SourceLocation.anon, [left, right])
    }

    if (node instanceof WhileStatementNode) {
      const test = this.compile(node.condition)
      const body = this.compile(node.body)
      return new WhileAst(VoidType, SourceLocation.anon, test, body)
    }

    compilerAssert(false, 'AST Not implemented', { ast: node })

  }

  getType(typeName: string) {
    const constant = this.getConstant(typeName)
    compilerAssert(constant, 'Constant not found', { type: typeName })
    compilerAssert(isType(constant), 'Not a type', { type: typeName })
    return constant
  }
}


const runTest = (node: ProgramNode) => {
  const codegen = new CodeGenerator()
  const c = new BasicCompiler(codegen)
  const ast = c.compile(node)
}

describe("compile", () => {
  it('testBasic', () => {
    // AST representing:
    // var result = 2
    // result = 3 + 2

    const ast = new ProgramNode([
      new VariableDeclarationNode('result', true, 'int', new LiteralNode(2)),
      new ExpressionStatementNode(new AssignmentNode(
        new IdentifierNode('result'),
        new BinaryExpressionNode('+', new LiteralNode(3), new LiteralNode(2))
      )),

    ]);
    runTest(ast)
  });

  it('testMembers', () => {
    // AST representing:
    // var p = Point{3, 4}
    // p.x = 5 + p.y

    const ast = new ProgramNode([
      new VariableDeclarationNode('p', true, 'Point', 
        new CreateStructNode('Point', 
          [new LiteralNode(3), new LiteralNode(4)]
        )
      ),
      new ExpressionStatementNode(new AssignmentNode(
        new MemberExpressionNode(new IdentifierNode('p'), 'int', 'x'),
        new BinaryExpressionNode('+', new LiteralNode(5), 
          new MemberExpressionNode(new IdentifierNode('p'), 'int', 'y'))
      )),

    ]);
    runTest(ast)
  });

  it('testFunc', () => {
    // AST representing:
    // var p = Point{3, 4}
    // fn foo(p: Point) {
    //   5 + p.y
    // }
    // foo(p)

    const ast = new ProgramNode([
      new VariableDeclarationNode('p', true, 'Point', 
        new CreateStructNode('Point', 
          [new LiteralNode(3), new LiteralNode(4)]
        )
      ),
      new FunctionDeclarationNode('foo', [
        new FunctionParameterNode('p', 'Point', Capability.Sink),
      ], 'void', new BlockStatementNode([
        new ExpressionStatementNode(new BinaryExpressionNode('+', new LiteralNode(5), 
          new MemberExpressionNode(new IdentifierNode('p'), 'int', 'y'))
        ),
      ])),
      new ExpressionStatementNode(
        new CallExpressionNode('foo', [new IdentifierNode('p')])
      ),

    ]);
    runTest(ast)
  });

})
