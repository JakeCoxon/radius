import { describe, it, expect } from "bun:test"
import { NumberAst, SourceLocation, IntType, Binding, Ast, VoidAst, VoidType, LetAst, isType, OperatorAst, SetAst, BindingAst, StatementsAst, TypeInfo, TypeField, ConcreteClassType, CompiledClass, ConstructorAst, FieldAst, SetFieldAst, FunctionDefinition, CompiledFunction, CallAst } from "../src/defs";
import { ASTNode, AndNode, AssignmentNode, BinaryExpressionNode, BlockStatementNode, BoolType, CallExpressionNode, Capability, CreateStructNode, ExpressionStatementNode, FunctionDeclarationNode, FunctionParameter, FunctionParameterNode, IdentifierNode, IfStatementNode, LetConstNode, LineType, LiteralNode, MemberExpressionNode, Module, PointType, ProgramNode, ReturnNode, VariableDeclarationNode, WhileStatementNode, compilerAssert, printIR, printLivenessMap, textColors } from "./defs";

class BasicCompiler {

  constants: Record<string, any> = {}
  variables: Record<string, Binding> = {}
  functions: Record<string, CompiledFunction> = {}
  
  constructor() {
    this.constants['int'] = IntType
    this.constants['bool'] = BoolType
    this.constants['void'] = VoidType
    this.defineType('Point', [
      { name: 'x', type: 'int' },
      { name: 'y', type: 'int' },
    ])
    this.defineType('Line', [
      { name: 'p1', type: 'Point' },
      { name: 'p2', type: 'Point' },
    ])
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
    this.constants[name] = classType
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
      this.constants[node.name] = node.value
      return new VoidAst(VoidType, SourceLocation.anon)
    }
    
    if (node instanceof VariableDeclarationNode) {
      const type = this.getType(node.type)
      const binding = new Binding(node.name, type)
      this.variables[node.name] = binding
      const value = node.initializer ? this.compile(node.initializer) : null
      return new LetAst(VoidType, SourceLocation.anon, binding, value)
    }
    
    if (node instanceof LiteralNode) {
      if (typeof node.value === 'number') {
        return new NumberAst(IntType, SourceLocation.anon, node.value)
      }
      compilerAssert(false, 'Not implemented', { ast: node })
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
      compilerAssert(left.type === right.type, 'Type mismatch', { left, right })
      if (left instanceof BindingAst) {
        return new SetAst(left.type, SourceLocation.anon, left.binding, right)
      }
      if (left instanceof FieldAst) {
        return new SetFieldAst(VoidType, SourceLocation.anon, left.left, left.field, right)
      }
      compilerAssert(false, 'Not implemented AssignmentNode', { left, right })
    }

    if (node instanceof IdentifierNode) {
      const binding = this.variables[node.name]
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
      compilerAssert(fieldIndex >= 0, 'Field not found', { object, field })
      return new FieldAst(type.typeInfo.fields[fieldIndex].fieldType, SourceLocation.anon, object, type.typeInfo.fields[fieldIndex])
    }

    if (node instanceof FunctionDeclarationNode) {
      compilerAssert(!this.functions[node.name], 'Function already defined', { ast: node })
      const body = this.compile(node.body)

      const binding = new Binding(node.name, VoidType)
      const returnType = VoidType
      const funcDef = null!
      const concreteTypes = node.params.map(x => this.getType(x.type))
      const argBindings = node.params.map((x, i) => new Binding(x.name, concreteTypes[i]))

      const compiledFunc = new CompiledFunction(binding, funcDef, returnType, concreteTypes, body, argBindings, [], 0)
      this.functions[node.name] = compiledFunc

      return new VoidAst(VoidType, SourceLocation.anon)
    }

    if (node instanceof CallExpressionNode) {
      const func = this.functions[node.callee]
      compilerAssert(func, 'Function not found', { ast: node })
      const args = node.args.map(x => this.compile(x))
      const binding = func.binding
      return new CallAst(func.returnType, SourceLocation.anon, binding, args, [])
    }

    compilerAssert(false, 'Not implemented', { ast: node })

  }

  getType(typeName: string) {
    compilerAssert(this.constants[typeName], 'Constant not found', { type: typeName })
    const t = this.constants[typeName]
    compilerAssert(isType(t), 'Not a type', { type: typeName })
    return t
  }
}


const runTest = (node: ProgramNode) => {
  const c = new BasicCompiler()
  const ast = c.compile(node)
  console.log(ast)
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
    new BlockStatementNode([])
  ),
  new FunctionDeclarationNode(
    'moveAssignLine',
    [
      new FunctionParameterNode('src', 'Line', Capability.Sink),
      new FunctionParameterNode('dst', 'Line', Capability.Inout),
    ],
    new BlockStatementNode([])
  ),
  new FunctionDeclarationNode(
    'moveInitPoint',
    [
      new FunctionParameterNode('src', 'Point', Capability.Sink),
      new FunctionParameterNode('dst', 'Point', Capability.Set),
    ],
    new BlockStatementNode([])
  ),
  new FunctionDeclarationNode(
    'moveAssignPoint',
    [
      new FunctionParameterNode('src', 'Point', Capability.Sink),
      new FunctionParameterNode('dst', 'Point', Capability.Inout),
    ],
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
      ], new BlockStatementNode([
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
