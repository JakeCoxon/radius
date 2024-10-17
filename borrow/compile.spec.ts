import { describe, it, expect } from "bun:test"
import { NumberAst, Binding, Ast, SetAst, BindingAst, StatementsAst, FieldAst, CallAst, Capability, Type } from "../src/defs";
import { AssignmentNode, BinaryExpressionNode, BlockStatementNode, CallExpressionNode, CreateStructNode, ExpressionStatementNode, FunctionDeclarationNode, FunctionParameterNode, IdentifierNode, LiteralNode, MemberExpressionNode, Module, ProgramNode, VariableDeclarationNode, printIR, printLivenessMap, textColors } from "./defs";
import { CodeGenerator } from "./codegen_ir";
import { BasicCompiler } from "./testUtils";

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
