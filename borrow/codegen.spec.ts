import { CodeGenerator } from "./codegen";
import { buildCFG, printCFG, printDominators } from "./controlflow";
import { AssignmentNode, BinaryExpressionNode, BlockStatementNode, ExpressionStatementNode, IdentifierNode, IfStatementNode, LiteralNode, MemberExpressionNode, ProgramNode, VariableDeclarationNode, WhileStatementNode, printIR } from "./defs";

function testUseOfUndeclaredVariable() {
  // AST representing: x = 5;
  const ast = new ProgramNode([
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('x'),
        new LiteralNode(5)
      )
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.error('testUseOfUndeclaredVariable failed: Expected an error but none was thrown.');
  } catch (error) {
    console.log(`testUseOfUndeclaredVariable passed: ${error.message}`);
  }
}

function testMultipleVariableDeclarations() {
  // AST representing:
  // let x;
  // let x;
  const ast = new ProgramNode([
    new VariableDeclarationNode('x'),
    new VariableDeclarationNode('x'),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.log('testMultipleVariableDeclarations passed.');
  } catch (error) {
    console.error(`testMultipleVariableDeclarations failed: ${error.message}`);
  }
}

function testNestedIfElseStatements() {
  // AST representing:
  // if (x > 0) {
  //   if (y < 0) {
  //     z = x + y;
  //   } else {
  //     z = x - y;
  //   }
  // } else {
  //   z = 0;
  // }
  const ast = new ProgramNode([
    new VariableDeclarationNode('x'),
    new VariableDeclarationNode('y'),
    new VariableDeclarationNode('z'),
    new IfStatementNode(
      new BinaryExpressionNode(
        '>',
        new IdentifierNode('x'),
        new LiteralNode(0)
      ),
      new BlockStatementNode([
        new IfStatementNode(
          new BinaryExpressionNode(
            '<',
            new IdentifierNode('y'),
            new LiteralNode(0)
          ),
          new BlockStatementNode([
            new ExpressionStatementNode(
              new AssignmentNode(
                new IdentifierNode('z'),
                new BinaryExpressionNode(
                  '+',
                  new IdentifierNode('x'),
                  new IdentifierNode('y')
                )
              )
            ),
          ]),
          new BlockStatementNode([
            new ExpressionStatementNode(
              new AssignmentNode(
                new IdentifierNode('z'),
                new BinaryExpressionNode(
                  '-',
                  new IdentifierNode('x'),
                  new IdentifierNode('y')
                )
              )
            ),
          ])
        ),
      ]),
      new BlockStatementNode([
        new ExpressionStatementNode(
          new AssignmentNode(
            new IdentifierNode('z'),
            new LiteralNode(0)
          )
        ),
      ])
    ),
  ]);

  const codeGenerator = new CodeGenerator();
  codeGenerator.generate(ast);
  console.log('testNestedIfElseStatements IR:');
  printIR(codeGenerator.blocks);
}

function testVariableShadowing() {
  // AST representing:
  // let x;
  // x = 1;
  // if (true) {
  //   let x;
  //   x = 2;
  // }
  // x = x + 1;
  const ast = new ProgramNode([
    new VariableDeclarationNode('x'),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('x'),
        new LiteralNode(1)
      )
    ),
    new IfStatementNode(
      new LiteralNode(true),
      new BlockStatementNode([
        new VariableDeclarationNode('x'),
        new ExpressionStatementNode(
          new AssignmentNode(
            new IdentifierNode('x'),
            new LiteralNode(2)
          )
        ),
      ])
    ),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('x'),
        new BinaryExpressionNode(
          '+',
          new IdentifierNode('x'),
          new LiteralNode(1)
        )
      )
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.log('testVariableShadowing IR:');
    printIR(codeGenerator.blocks);
  } catch (error) {
    console.error(`testVariableShadowing failed: ${error.message}`);
  }
}

function testFieldAccessOnNonObject() {
  // AST representing:
  // let x;
  // x = 5;
  // y = x.field;
  const ast = new ProgramNode([
    new VariableDeclarationNode('x'),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('x'),
        new LiteralNode(5)
      )
    ),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('y'),
        new MemberExpressionNode(
          new IdentifierNode('x'),
          'field'
        )
      )
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.error('testFieldAccessOnNonObject failed: Expected an error but none was thrown.');
  } catch (error) {
    console.log(`testFieldAccessOnNonObject passed: ${error.message}`);
  }
}

function testUseOfUninitializedVariable() {
  // AST representing:
  // let x;
  // y = x + 1;
  const ast = new ProgramNode([
    new VariableDeclarationNode('x'),
    new VariableDeclarationNode('y'),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('y'),
        new BinaryExpressionNode(
          '+',
          new IdentifierNode('x'),
          new LiteralNode(1)
        )
      )
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.error('testUseOfUninitializedVariable failed: Expected an error but none was thrown.');
  } catch (error) {
    console.log(`testUseOfUninitializedVariable passed: ${error.message}`);
  }
}

function testComplexExpressions() {
  // AST representing:
  // let x;
  // x = (5 + 3) * (2 - 1);
  const ast = new ProgramNode([
    new VariableDeclarationNode('x'),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('x'),
        new BinaryExpressionNode(
          '*',
          new BinaryExpressionNode(
            '+',
            new LiteralNode(5),
            new LiteralNode(3)
          ),
          new BinaryExpressionNode(
            '-',
            new LiteralNode(2),
            new LiteralNode(1)
          )
        )
      )
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.log('testComplexExpressions IR:');
    printIR(codeGenerator.blocks);
  } catch (error) {
    console.error(`testComplexExpressions failed: ${error.message}`);
  }
}

function testWhileLoopThatMightNotExecute() {
  // AST representing:
  // let x;
  // x = 0;
  // while (x > 0) {
  //   x = x - 1;
  // }
  const ast = new ProgramNode([
    new VariableDeclarationNode('x'),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('x'),
        new LiteralNode(0)
      )
    ),
    new WhileStatementNode(
      new BinaryExpressionNode(
        '>',
        new IdentifierNode('x'),
        new LiteralNode(0)
      ),
      new BlockStatementNode([
        new ExpressionStatementNode(
          new AssignmentNode(
            new IdentifierNode('x'),
            new BinaryExpressionNode(
              '-',
              new IdentifierNode('x'),
              new LiteralNode(1)
            )
          )
        ),
      ])
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.log('testWhileLoopThatMightNotExecute IR:');
    printIR(codeGenerator.blocks);
    const cfg = buildCFG(codeGenerator.blocks)
    printCFG(cfg)
    printDominators(cfg)
  } catch (error) {
    console.error(`testWhileLoopThatMightNotExecute failed: ${error.message}`);
  }
}

function testAssignmentToUndeclaredVariable() {
  // AST representing:
  // y = 5;
  const ast = new ProgramNode([
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('y'),
        new LiteralNode(5)
      )
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.error('testAssignmentToUndeclaredVariable failed: Expected an error but none was thrown.');
  } catch (error) {
    console.log(`testAssignmentToUndeclaredVariable passed: ${error.message}`);
  }
}

function testEmptyProgram() {
  // AST representing an empty program
  const ast = new ProgramNode([]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.log('testEmptyProgram passed.');
  } catch (error) {
    console.error(`testEmptyProgram failed: ${error.message}`);
  }
}

function testEmptyBlockStatement() {
  // AST representing:
  // if (true) {
  //   // empty block
  // } else {
  //   // empty block
  // }
  const ast = new ProgramNode([
    new IfStatementNode(
      new LiteralNode(true),
      new BlockStatementNode([]),
      new BlockStatementNode([])
    ),
  ]);

  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.log('testEmptyBlockStatement passed.');
  } catch (error) {
    console.error(`testEmptyBlockStatement failed: ${error.message}`);
  }
}

function runAllTests() {
  testUseOfUndeclaredVariable();
  testMultipleVariableDeclarations();
  testNestedIfElseStatements();
  testVariableShadowing();
  // testFieldAccessOnNonObject();
  // testUseOfUninitializedVariable();
  testComplexExpressions();
  testWhileLoopThatMightNotExecute();
  // testAssignmentToUndeclaredVariable();
  testEmptyProgram();
  testEmptyBlockStatement();
}

runAllTests()
