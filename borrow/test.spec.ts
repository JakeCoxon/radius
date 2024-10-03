import { describe, it, expect } from "bun:test"
import { CodeGenerator } from "./codegen";
import { buildCFG, printCFG, printDominators } from "./controlflow";
import { AndNode, AssignmentNode, BinaryExpressionNode, BlockStatementNode, BoolType, CallExpressionNode, CreateStructNode, ExpressionStatementNode, FunctionDeclarationNode, FunctionParameter, FunctionParameterNode, IdentifierNode, IfStatementNode, IntType, LetConstNode, LiteralNode, MemberExpressionNode, PointType, ProgramNode, ReturnNode, VariableDeclarationNode, WhileStatementNode, printIR, printLivenessMap, textColors } from "./defs";
import { ExclusivityCheckingPass } from "./exclusivity";
import { InitializationCheckingPass } from "./initialization";
import { insertCloseAccesses } from "./liveness";
import { ReifyAccessPass } from "./reifyaccess";


const runTest = (name: string, ast: ProgramNode) => {
  console.log(textColors.green(`\n\n#### Begin ${name} ####`));
  const codeGenerator = new CodeGenerator();

  try {
    codeGenerator.generate(ast);   
    console.log(codeGenerator.functionBlocks)
    console.log("")

    for (const fn of codeGenerator.functionBlocks) {
      console.log(textColors.yellow(`\n// ${fn.name} ///////////////////////////////////////////////////////////\n`));
      
      printIR(fn.blocks);

      const cfg = buildCFG(fn.blocks)
      printCFG(cfg)
      printDominators(cfg)

      const reify = new ReifyAccessPass(cfg);
      reify.debugLog = true;
      reify.reifyAccesses();

      console.log("Reified")
      printIR(fn.blocks);

      const interpreter = new InitializationCheckingPass(fn);
      interpreter.debugLog = true;
      interpreter.checkedInterpret();

      console.log("")
      insertCloseAccesses(cfg, fn.blocks)

      const interpreter2 = new ExclusivityCheckingPass(fn)
      interpreter2.debugLog = true;
      interpreter2.checkedInterpret();
      console.log("")

      console.log(``);
      printIR(fn.blocks);
    }


  } catch (error) {
    console.error(`${name} failed: ${error.message}`);
    console.error(error.stack)
  }
}

describe("integration", () => {
  it('testFunction', () => {
    // AST representing:
    // function add(a, b) {
    //   return a + b;
    // }
    // let result;
    // result = add(2, 3);
    
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
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
  })

  it('testStruct', () => {
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
    runTest("testStruct", ast)
  })

  it('testControlFlow', () => {
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
  })

  it('testWhileLoop', () => {
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
  })

  it('testWhileLoopUninitialized', () => {
    // AST representing:
    // var x: int
    // var y: int
    // x = 0
    // while (x < 10) {
    //   y = x
    //   x = x + 1
    // }
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('x', true, 'int'),
      new VariableDeclarationNode('y', true, 'int'),
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
              new IdentifierNode('y'),
              new IdentifierNode('x')
            )
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
          )
        ])
      )
    ]);
    // TODO: This should fail
    runTest("testWhileLoop", ast)
  })

  it('testSimpleReassignment', () => {
    // let y: int
    // y = 5
    // y = y + 2
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('y', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('y'),
          new LiteralNode(5)
        )
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('y'),
          new BinaryExpressionNode(
            '+',
            new IdentifierNode('y'),
            new LiteralNode(2)
          )
        )
      )
    ]);
    runTest("testSimpleReassignment", ast);
  })

  it('testNestedLoops', () => {
    // let a: int, b: int
    // a = 0
    // b = 5
    // while (a < 10) {
    //   a = a + 1
    //   while (b > 0) {
    //     b = b - 1
    //   }
    // }
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('a', true, 'int'),
      new VariableDeclarationNode('b', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('a'),
          new LiteralNode(0)
        )
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('b'),
          new LiteralNode(5)
        )
      ),
      new WhileStatementNode(
        new BinaryExpressionNode(
          '<',
          new IdentifierNode('a'),
          new LiteralNode(10)
        ),
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('a'),
              new BinaryExpressionNode(
                '+',
                new IdentifierNode('a'),
                new LiteralNode(1)
              )
            )
          ),
          new WhileStatementNode(
            new BinaryExpressionNode(
              '>',
              new IdentifierNode('b'),
              new LiteralNode(0)
            ),
            new BlockStatementNode([
              new ExpressionStatementNode(
                new AssignmentNode(
                  new IdentifierNode('b'),
                  new BinaryExpressionNode(
                    '-',
                    new IdentifierNode('b'),
                    new LiteralNode(1)
                  )
                )
              )
            ])
          )
        ])
      )
    ]);
    runTest("testNestedLoops", ast);
  })

  it('testIfElseBranches', () => {
    // let z: int, w: int
    // z = 10
    // if (z > 5) {
    //   w = z - 5
    // } else {
    //   w = z + 5
    // }
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('z', true, 'int'),
      new VariableDeclarationNode('w', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('z'),
          new LiteralNode(10)
        )
      ),
      new IfStatementNode(
        new BinaryExpressionNode(
          '>',
          new IdentifierNode('z'),
          new LiteralNode(5)
        ),
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('w'),
              new BinaryExpressionNode(
                '-',
                new IdentifierNode('z'),
                new LiteralNode(5)
              )
            )
          )
        ]),
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('w'),
              new BinaryExpressionNode(
                '+',
                new IdentifierNode('z'),
                new LiteralNode(5)
              )
            )
          )
        ])
      )
    ]);
    runTest("testIfElseBranches", ast);
  })

  it.skip('testLoopWithBreakContinue', () => {
    // let i: int
    // i = 0
    // while (i < 10) {
    //   if (i == 5) {
    //     break;
    //   } else {
    //     i = i + 1;
    //     continue;
    //   }
    // }
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('i', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('i'),
          new LiteralNode(0)
        )
      ),
      new WhileStatementNode(
        new BinaryExpressionNode(
          '<',
          new IdentifierNode('i'),
          new LiteralNode(10)
        ),
        new BlockStatementNode([
          new IfStatementNode(
            new BinaryExpressionNode(
              '==',
              new IdentifierNode('i'),
              new LiteralNode(5)
            ),
            new BlockStatementNode([
              new BreakStatementNode()
            ]),
            new BlockStatementNode([
              new ExpressionStatementNode(
                new AssignmentNode(
                  new IdentifierNode('i'),
                  new BinaryExpressionNode(
                    '+',
                    new IdentifierNode('i'),
                    new LiteralNode(1)
                  )
                )
              ),
              new ContinueStatementNode()
            ])
          )
        ])
      )
    ]);
    runTest("testLoopWithBreakContinue", ast);
  })

  it('testMultipleScopeVariableUsage', () => {
    // let a: int
    // a = 1
    // while (a < 5) {
    //   let b: int
    //   b = a + 2
    //   a = b
    // }
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('a', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('a'),
          new LiteralNode(1)
        )
      ),
      new WhileStatementNode(
        new BinaryExpressionNode(
          '<',
          new IdentifierNode('a'),
          new LiteralNode(5)
        ),
        new BlockStatementNode([
          new VariableDeclarationNode('b', true, 'int'),
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('b'),
              new BinaryExpressionNode(
                '+',
                new IdentifierNode('a'),
                new LiteralNode(2)
              )
            )
          ),
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('a'),
              new IdentifierNode('b')
            )
          )
        ])
      )
    ]);
    runTest("testMultipleScopeVariableUsage", ast);
  })

  it('testConditionalReassignmentAcrossBlocks', () => {
    // let x: int
    // x = 10
    // if (x > 5) {
    //   x = x + 5  // Block 1
    // } else {
    //   x = x - 5  // Block 2
    // }
    // x = x * 2    // Block 3 (post-branch)
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('x', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('x'),
          new LiteralNode(10)
        )
      ),
      new IfStatementNode(
        new BinaryExpressionNode(
          '>',
          new IdentifierNode('x'),
          new LiteralNode(5)
        ),
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('x'),
              new BinaryExpressionNode(
                '+',
                new IdentifierNode('x'),
                new LiteralNode(5)
              )
            )
          )
        ]),
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('x'),
              new BinaryExpressionNode(
                '-',
                new IdentifierNode('x'),
                new LiteralNode(5)
              )
            )
          )
        ])
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('x'),
          new BinaryExpressionNode(
            '*',
            new IdentifierNode('x'),
            new LiteralNode(2)
          )
        )
      )
    ]);
    runTest("testConditionalReassignmentAcrossBlocks", ast);
  })


  it('testLoopWithEarlyReturn', () => {
    // let x: int
    // x = 0
    // while (x < 10) {
    //   if (x == 5) {
    //     return
    //   }
    //   x = x + 1    // Block 1 (inside loop)
    // }              // Block 2 (after loop)
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
          new IfStatementNode(
            new BinaryExpressionNode(
              '==',
              new IdentifierNode('x'),
              new LiteralNode(5)
            ),
            new BlockStatementNode([
              new ReturnNode()
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
          )
        ])
      )
    ]);
    runTest("testLoopWithEarlyReturn", ast);
  })

  it('testLogicalAnd', () => {
    // let a: int, b: int
    // a = 10
    // b = 5
    // if (a > 0 && b < 10) {
    //   a = a + b  // Block 1
    // }
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('a', true, 'int'),
      new VariableDeclarationNode('b', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('a'),
          new LiteralNode(10)
        )
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('b'),
          new LiteralNode(5)
        )
      ),
      new IfStatementNode(
        new AndNode(
          new BinaryExpressionNode('>', new IdentifierNode('a'), new LiteralNode(0)),
          new BinaryExpressionNode('<', new IdentifierNode('b'), new LiteralNode(10))
        ),
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new IdentifierNode('a'),
              new BinaryExpressionNode('+', new IdentifierNode('a'), new IdentifierNode('b'))
            )
          )
        ])
      )
    ]);
    runTest("testLogicalAnd", ast);
  })

  it('testLogicalAndInLoop', () => {
    // let x: int, y: int
    // x = 0
    // y = 10
    // while (x < 10) {
    //   if (x > 5 && y < 15) {
    //     y = y + 1    // Block 1
    //   }
    //   x = x + 1      // Block 2
    // }
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new VariableDeclarationNode('x', true, 'int'),
      new VariableDeclarationNode('y', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('x'),
          new LiteralNode(0)
        )
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('y'),
          new LiteralNode(10)
        )
      ),
      new WhileStatementNode(
        new BinaryExpressionNode('<', new IdentifierNode('x'), new LiteralNode(10)),
        new BlockStatementNode([
          new IfStatementNode(
            new AndNode(
              new BinaryExpressionNode('>', new IdentifierNode('x'), new LiteralNode(5)),
              new BinaryExpressionNode('<', new IdentifierNode('y'), new LiteralNode(15))
            ),
            new BlockStatementNode([
              new ExpressionStatementNode(
                new AssignmentNode(
                  new IdentifierNode('y'),
                  new BinaryExpressionNode(
                    '+',
                    new IdentifierNode('y'),
                    new LiteralNode(1)
                  )
                )
              )
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
          )
        ])
      )
    ]);
    runTest("testLogicalAndInLoop", ast);
  })

  it('testLogicalAndInFunctionCall', () => {
    // let x: int
    // x = 1
    // foo(x, x > 0 && x < 10)
    const ast = new ProgramNode([
      new LetConstNode('int', IntType),
      new LetConstNode('bool', BoolType),

      new FunctionDeclarationNode(
        'foo',
        [
          new FunctionParameterNode('z', 'int', false),
          new FunctionParameterNode('w', 'bool', false),
        ],
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('x', true, 'int'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('x'),
          new LiteralNode(1)
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'foo',
          [
            new IdentifierNode('x'),  // First argument: x
            new AndNode(              // Second argument: x > 0 && x < 10
              new BinaryExpressionNode(
                '>',
                new IdentifierNode('x'),
                new LiteralNode(0)
              ),
              new BinaryExpressionNode(
                '<',
                new IdentifierNode('x'),
                new LiteralNode(10)
              )
            )
          ]
        )
      )
    ]);
    runTest("testLogicalAndInFunctionCall", ast);
  })

  it('testStructFieldConditional', () => {
    // AST representing:
    // function modifyIfPositive(p: inout Point, x: int) {
    //   if (x > 0) {
    //     p.x = p.x + x;
    //   }
    // }
    // let point = Point{2, 3};
    // modifyIfPositive(point, 3);
    
    const ast = new ProgramNode([
      new LetConstNode('Point', PointType),
      new LetConstNode('int', IntType),
      new FunctionDeclarationNode(
        'modifyIfPositive',
        [
          new FunctionParameterNode('p', 'Point', true), 
          new FunctionParameterNode('x', 'int', false),
        ],
        new BlockStatementNode([
          new IfStatementNode(
            new BinaryExpressionNode('>', new IdentifierNode('x'), new LiteralNode(0)),
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
          )
        ])
      ),
      new VariableDeclarationNode('point', true, 'Point'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('point'),
          new CreateStructNode(
            'Point',
            [new LiteralNode(2), new LiteralNode(3)]
          )
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'modifyIfPositive',
          [new IdentifierNode('point'), new LiteralNode(3)]
        )
      )
    ]);
    runTest("testStructFieldConditional", ast);
  })

  it('testStructMultipleCalls', () => {
    // AST representing:
    // function addX(p: inout Point, x: int) { p.x = p.x + x; }
    // function addY(p: inout Point, y: int) { p.y = p.y + y; }
    // let result = Point{2, 3};
    // addX(result, 3);
    // addY(result, 2);

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
      new FunctionDeclarationNode(
        'addY',
        [
          new FunctionParameterNode('p', 'Point', true), 
          new FunctionParameterNode('y', 'int', false),
        ],
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'y'),
              new BinaryExpressionNode(
                '+',
                new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'y'),
                new IdentifierNode('y')
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
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'addY',
          [new IdentifierNode('result'), new LiteralNode(2)]
        )
      )
    ]);
    runTest("testStructMultipleCalls", ast);
  })

  it.skip('testNestedStruct', () => {
    // AST representing:
    // struct Inner { int z; }
    // struct Outer { Inner inner; }
    // function modifyInner(p: inout Outer) {
    //   p.inner.z = p.inner.z + 1;
    // }
    // let result = Outer{Inner{5}};
    // modifyInner(result);

    const ast = new ProgramNode([
      new LetConstNode('Inner', InnerType),
      new LetConstNode('Outer', OuterType),
      new LetConstNode('int', IntType),
      new FunctionDeclarationNode(
        'modifyInner',
        [new FunctionParameterNode('p', 'Outer', true)],
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new MemberExpressionNode(
                new MemberExpressionNode(new IdentifierNode('p'), 'Outer', 'inner'),
                'Inner', 'z'
              ),
              new BinaryExpressionNode(
                '+',
                new MemberExpressionNode(
                  new MemberExpressionNode(new IdentifierNode('p'), 'Outer', 'inner'),
                  'Inner', 'z'
                ),
                new LiteralNode(1)
              )
            )
          )
        ])
      ),
      new VariableDeclarationNode('result', true, 'Outer'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('result'),
          new CreateStructNode(
            'Outer',
            [new CreateStructNode('Inner', [new LiteralNode(5)])]
          )
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'modifyInner',
          [new IdentifierNode('result')]
        )
      )
    ]);
    runTest("testNestedStruct", ast);
  })
})