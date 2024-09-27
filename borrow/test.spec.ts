import { CodeGenerator } from "./codegen";
import { buildCFG, printCFG, printDominators } from "./controlflow";
import { AssignmentNode, BinaryExpressionNode, BlockStatementNode, CallExpressionNode, CreateStructNode, ExpressionStatementNode, FunctionDeclarationNode, FunctionParameter, FunctionParameterNode, IdentifierNode, IfStatementNode, IntType, LetConstNode, LiteralNode, MemberExpressionNode, PointType, ProgramNode, ReturnNode, VariableDeclarationNode, WhileStatementNode, printIR } from "./defs";
import { InitializationCheckingPass } from "./initialization";


function testWhileLoopThatMightNotExecute() {
  // AST representing:
  // let x;
  // x = 0;
  // while (x > 0) {
  //   x = x - 1;
  // }
  const ast = new ProgramNode([
    new VariableDeclarationNode('x', true, 'int'),
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

    // const interpreter = new InitializationCheckingPass(codeGenerator.blocks);
    // interpreter.interpret();
  } catch (error) {
    console.error(`testWhileLoopThatMightNotExecute failed: ${error.message}`);
  }
}

const runTest = (name: string, ast: ProgramNode) => {
  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);
    console.log(`${name} IR:`);
    for (const fn of codeGenerator.functionBlocks) {
      console.log(``);
      console.log(`Function ${fn.name}:`);
      printIR(fn.blocks);
    }
    console.log("")
    const cfg = buildCFG(codeGenerator.blocks)
    printCFG(cfg)
    printDominators(cfg)

    console.log(codeGenerator.functionBlocks)
    console.log("")

    for (const fn of codeGenerator.functionBlocks) {
      console.log(`\n// ${fn.name} ///////////////////////////////////////////////////////////\n`);
      const interpreter = new InitializationCheckingPass(fn);
      interpreter.checkedInterpret();
    }
  } catch (error) {
    console.error(`${name} failed: ${error.message}`);
  }
}


function testFunction() {
  // AST representing:
  // function add(a, b) {
  //   return a + b;
  // }
  // let result;
  // result = add(2, 3);
  
  const ast = new ProgramNode([
    new FunctionDeclarationNode(
      'add',
      [
        new FunctionParameterNode('a', 'int', true), 
        new FunctionParameterNode('b', 'int', false)
      ],
      new BlockStatementNode([
        new ReturnNode(
          new BinaryExpressionNode(
            '+',
            new IdentifierNode('a'),
            new IdentifierNode('b')
          )
        )
      ])
    ),
    new VariableDeclarationNode('result', true, 'int'),
    new VariableDeclarationNode('foo', true, 'int'),
    new ExpressionStatementNode(
      new AssignmentNode(new IdentifierNode('foo'), new LiteralNode(2)),
    ),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('result'),
        new CallExpressionNode(
          'add',
          [new IdentifierNode('foo'), new LiteralNode(3)]
        )
      )
    ),
  ]);
  runTest("testFunction", ast)
}

function testStruct() {
  // AST representing:
  // function addX(p: inout Point, x: int) {
  //   p.x = p.x + x;
  // }
  // let result = Point{2, 3};
  // addX(result, 3);
  
  const ast = new ProgramNode([
    new LetConstNode('Point', PointType),
    new LetConstNode('int', IntType),
    new FunctionDeclarationNode(
      'addX',
      [
        new FunctionParameterNode('p', 'Point', true), 
        new FunctionParameterNode('x', 'int', false),
      ],
      new BlockStatementNode([
        new ExpressionStatementNode(
          new AssignmentNode(
            new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'x'),
            new BinaryExpressionNode(
              '+',
              new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'x'),
              new IdentifierNode('x')
            )
          )
        )
      ])
    ),
    new VariableDeclarationNode('result', true, 'Point'),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('result'),
        new CreateStructNode(
          'Point',
          [new LiteralNode(2), new LiteralNode(3)]
        )
      )
    ),
    new ExpressionStatementNode(
      new CallExpressionNode(
        'addX',
        [new IdentifierNode('result'), new LiteralNode(3)]
      )
    )
  ]);
  runTest("testFunction", ast)
}

function testControlFlow() {
  // AST representing:
  // let x: int
  // if (1) {
  //   x = 1;
  // } else {
  //   x = 2;
  // }
  // use(x + 3)
  const ast = new ProgramNode([
    new LetConstNode('int', IntType),
    new VariableDeclarationNode('x', true, 'int'),
    new FunctionDeclarationNode(
      'use',
      [
        new FunctionParameterNode('z', 'int', false),
      ],
      new BlockStatementNode([])
    ),
    new IfStatementNode(
      new LiteralNode(1),
      new BlockStatementNode([
        new ExpressionStatementNode(
          new AssignmentNode(
            new IdentifierNode('x'),
            new LiteralNode(1)
          )
        )
      ]),
      new BlockStatementNode([
        new ExpressionStatementNode(
          new AssignmentNode(
            new IdentifierNode('x'),
            new LiteralNode(2)
          )
        )
      ])
    ),
    new ExpressionStatementNode(
      new CallExpressionNode('use', [
        new BinaryExpressionNode(
          '+',
          new IdentifierNode('x'),
          new LiteralNode(3)
        )
      ])
    )
  ]);
  runTest("testControlFlow", ast)
}

function testWhileLoop() {
  // AST representing:
  // let x: int
  // x = 0
  // while (x < 10) {
  //   x = x + 1
  // }
  const ast = new ProgramNode([
    new LetConstNode('int', IntType),
    new VariableDeclarationNode('x', true, 'int'),
    new ExpressionStatementNode(
      new AssignmentNode(
        new IdentifierNode('x'),
        new LiteralNode(0)
      )
    ),
    new WhileStatementNode(
      new BinaryExpressionNode(
        '<',
        new IdentifierNode('x'),
        new LiteralNode(10)
      ),
      new BlockStatementNode([
        new ExpressionStatementNode(
          new AssignmentNode(
            new IdentifierNode('x'),
            new BinaryExpressionNode(
              '+',
              new IdentifierNode('x'),
              new LiteralNode(1)
            )
          )
        )
      ])
    )
  ]);
  runTest("testWhileLoop", ast)
}

// testFunction()
testStruct()
testControlFlow()
testWhileLoop()
// testWhileLoopThatMightNotExecute()