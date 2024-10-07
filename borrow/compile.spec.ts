import { describe, it, expect } from "bun:test"
import { NumberAst, SourceLocation, IntType, Binding, Ast, VoidAst, VoidType, LetAst, isType, OperatorAst, SetAst, BindingAst, StatementsAst, TypeInfo, TypeField, ConcreteClassType, CompiledClass, ConstructorAst, FieldAst, SetFieldAst, FunctionDefinition, CompiledFunction, CallAst, ReturnAst, IfAst, BoolType, AndAst, WhileAst, FunctionParameter, Capability } from "../src/defs";
import { ASTNode, AndNode, AssignmentNode, BinaryExpressionNode, BlockStatementNode, CallExpressionNode, CreateStructNode, ExpressionStatementNode, FunctionDeclarationNode, FunctionParameterNode, IdentifierNode, IfStatementNode, LetConstNode, LiteralNode, MemberExpressionNode, Module, ProgramNode, ReturnNode, VariableDeclarationNode, WhileStatementNode, compilerAssert, printIR, printLivenessMap, textColors } from "./defs";

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
  
  constructor() {
    this.defineConstant('int', IntType)
    this.defineConstant('bool', BoolType)
    this.defineConstant('void', VoidType)
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
    const typeInfo: TypeInfo = { sizeof: 0, fields: typeFields, metaobject: {}, isReferenceType: true }
    const binding = new Binding(name, VoidType)
    const compiledClass = new CompiledClass(SourceLocation.anon, name, binding, null!, null!, null!, typeFields, [], 0)
    let classType = new ConcreteClassType(compiledClass, typeInfo)
    fields.forEach((f, i) => {
      const type = this.getType(f.type)
      typeFields[i] = new TypeField(SourceLocation.anon, f.name, classType, i, type)
    })
    this.defineConstant(name, classType)
    return classType
  }

  compile(node: ASTNode): Ast {

    if (node instanceof ProgramNode) {
      const program = node.body.map(x => this.compile(x))
      return new StatementsAst(VoidType, SourceLocation.anon, program)
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
      const funcDef = null!

      const parameters = argBindings.map((x, i) => new FunctionParameter(x, x.type, node.params[i].capability))

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
  const c = new BasicCompiler()
  const ast = c.compile(node)
}

const common = [
  // new LetConstNode('Point', PointType),
  // new LetConstNode('Line', LineType),
  new LetConstNode('int', IntType),
  new FunctionDeclarationNode(
    'moveInitLine',
    [
      new FunctionParameterNode('src', 'Line', Capability.Sink),
      new FunctionParameterNode('dst', 'Line', Capability.Set),
    ],
    'void',
    new BlockStatementNode([])
  ),
  new FunctionDeclarationNode(
    'moveAssignLine',
    [
      new FunctionParameterNode('src', 'Line', Capability.Sink),
      new FunctionParameterNode('dst', 'Line', Capability.Inout),
    ],
    'void',
    new BlockStatementNode([])
  ),
  new FunctionDeclarationNode(
    'moveInitPoint',
    [
      new FunctionParameterNode('src', 'Point', Capability.Sink),
      new FunctionParameterNode('dst', 'Point', Capability.Set),
    ],
    'void',
    new BlockStatementNode([])
  ),
  new FunctionDeclarationNode(
    'moveAssignPoint',
    [
      new FunctionParameterNode('src', 'Point', Capability.Sink),
      new FunctionParameterNode('dst', 'Point', Capability.Inout),
    ],
    'void',
    new BlockStatementNode([])
  ),
]

describe("compile", () => {
  it('testBasic', () => {
    // AST representing:
    // var result = 2
    // result = 3 + 2

    const ast = new ProgramNode([
      ...common,
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
      ...common,
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
      ...common,
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
