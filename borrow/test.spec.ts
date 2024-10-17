import { describe, it, expect } from "bun:test"
import { CodeGenerator } from "./codegen_ir";
import { buildCFG, printCFG, printDominators } from "./controlflow";
import { AndNode, AssignmentNode, BinaryExpressionNode, BlockStatementNode, BreakStatementNode, CallExpressionNode, ContinueStatementNode, CreateStructNode, ExpressionStatementNode, FunctionBlock, FunctionDeclarationNode, FunctionParameterNode, IdentifierNode, IfStatementNode, LiteralNode, MemberExpressionNode, Module, BuiltinNode, ProgramNode, ReturnNode, VariableDeclarationNode, WhileStatementNode, compilerAssert, printIR, printLivenessMap, textColors } from "./defs";
import { ExclusivityCheckingPass } from "./exclusivity";
import { InitializationCheckingPass } from "./initialization";
import { insertCloseAccesses } from "./liveness";
import { ReifyAccessPass } from "./reifyaccess";
import { BasicCompiler } from "./testUtils";
import { Binding, Capability, CompiledFunction, FunctionParameter, GlobalCompilerState, IntType, VoidType } from "../src/defs";
import { writeLlvmBytecode } from "./codegen_llvm";
import { externalBuiltinBindings } from "../src/compiler_sugar";

const DebugLog = true

const runMandatoryPasses = (codeGenerator: CodeGenerator, mod: Module, fn: FunctionBlock) => {
  console.log(textColors.yellow(`\n// ${fn.name} ///////////////////////////////////////////////////////////\n`));

  // Filter out blocks that are not reachable from the entry block
  const cfgFirst = buildCFG(fn.blocks)
  fn.blocks = cfgFirst.blocks.filter(b => cfgFirst.predecessors.get(b)!.length > 0 || b === cfgFirst.entry)
  
  printIR(fn.blocks);

  const cfg = buildCFG(fn.blocks)
  printCFG(cfg)
  printDominators(cfg)

  const reify = new ReifyAccessPass(cfg);
  reify.debugLog = DebugLog;
  reify.reifyAccesses();

  console.log("Reified")
  printIR(fn.blocks);

  const interpreter = new InitializationCheckingPass(codeGenerator, mod, fn);
  interpreter.debugLog = DebugLog;
  interpreter.checkedInterpret();

  console.log("Initialized")
  printIR(fn.blocks);

  console.log("")
  insertCloseAccesses(cfg, fn.blocks, DebugLog)

  console.log("Closed access")
  printIR(fn.blocks);

  const interpreter2 = new ExclusivityCheckingPass(fn)
  interpreter2.debugLog = DebugLog;
  interpreter2.checkedInterpret();
  console.log("")

  console.log(``);
  printIR(fn.blocks);
}
const runTest = (name: string, node: ProgramNode) => {
  try {
    console.log(textColors.green(`\n\n#### Begin ${name} ####`));
    const codeGenerator = new CodeGenerator();
    const printInt = externalBuiltinBindings.printInt
    const printArg = new Binding('printArg', VoidType)
    codeGenerator.functions.set(printInt,
      new CompiledFunction(printInt, {} as any, VoidType, [IntType], null!, [printArg], [
        new FunctionParameter(printArg, IntType, false, IntType, Capability.Let)
      ], [], 0))

    const compiler = new BasicCompiler(codeGenerator)
    compiler.compile(node)

    for (const fn of compiler.allFunctions.values()) {
      codeGenerator.functions.set(fn.binding, fn);
    }

    const globalCompilerState: GlobalCompilerState = {
      compiledIr: new Map(),
      externalDefinitions: [],
      initializerFunctionBinding: new Binding('initializer', VoidType),
      exports: {},
      globalLets: [],
      compiledFunctions: new Map(),
    }

    const mod = new Module()
    mod.functionMap = codeGenerator.functions

    for (const fn of codeGenerator.functions.values()) {
      if (!fn.body) continue
      const ir = codeGenerator.generateFunction(fn.binding, fn.parameters, fn.returnType, fn.body)
      runMandatoryPasses(codeGenerator, mod, ir)

      globalCompilerState.compiledIr.set(fn.binding, ir)
    }


    globalCompilerState.compiledFunctions = codeGenerator.functions

    const file = Bun.file(`./output/${name}.ll`)
    const bytecodeWriter = file.writer()

    writeLlvmBytecode(globalCompilerState, bytecodeWriter)
    bytecodeWriter.end()


  } catch (error) {
    console.error(`${name} failed: ${error.message}`);
    console.error(error.stack)
    console.error(error.info)
    throw error;
  }
}

describe("integration", () => {
  it('testFunction', () => {
    // AST representing:
    // function add(a: int, b: int) {
    //   return a + b;
    // }
    // var result;
    // var foo;
    // foo = 2;
    // result = add(foo, 3);
    
    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'add',
        [
          new FunctionParameterNode('a', 'int', Capability.Let), 
          new FunctionParameterNode('b', 'int', Capability.Let)
        ],
        'int',
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
    // var result = Point{2, 3};
    // addX(result, 3);
    
    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'addX',
        [
          new FunctionParameterNode('p', 'Point', Capability.Inout), 
          new FunctionParameterNode('x', 'int', Capability.Let),
        ],
        'void',
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
    // var x: int
    // if (1) {
    //   x = 1;
    // } else {
    //   x = 2;
    // }
    // use(x + 3)
    const ast = new ProgramNode([
      new VariableDeclarationNode('x', true, 'int'),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('z', 'int', Capability.Let),
        ],
        'void',
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

  it('testControlFlowImmutable', () => {
    // AST representing:
    // let x: int = 0
    // if (1) {
    //   x = 1;
    // } else {
    //   x = 2;
    // }
    const ast = new ProgramNode([
      new VariableDeclarationNode('x', false, 'int',
        new LiteralNode(0)
      ),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('z', 'int', Capability.Let),
        ],
        'void',
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
      )
    ]);
    expect(() => runTest("testControlFlowImmutable", ast)).toThrow("Cannot assign")
  })

  it('testWhileLoop', () => {
    // AST representing:
    // let x: int
    // x = 0
    // while (x < 10) {
    //   x = x + 1
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

  it.skip('testWhileLoopUninitialized', () => {
    // AST representing:
    // var x: int
    // var y: int
    // x = 0
    // while (x < 10) {
    //   y = x
    //   x = x + 1
    // }
    const ast = new ProgramNode([
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
    expect(() => runTest("testWhileLoopUninitialized", ast)).toThrow("Uninitialized")
  })

  it('testSimpleReassignment', () => {
    // var y: int
    // y = 5
    // y = y + 2
    const ast = new ProgramNode([
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
    // var a: int, b: int
    // a = 0
    // b = 5
    // while (a < 10) {
    //   a = a + 1
    //   while (b > 0) {
    //     b = b - 1
    //   }
    // }
    const ast = new ProgramNode([
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
    // var z: int, w: int
    // z = 10
    // if (z > 5) {
    //   w = z - 5
    // } else {
    //   w = z + 5
    // }
    const ast = new ProgramNode([
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

  it('testLoopWithBreakContinue', () => {
    // var i: int
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
    // var a: int
    // a = 1
    // while (a < 5) {
    //   var b: int
    //   b = a + 2
    //   a = b
    // }
    const ast = new ProgramNode([
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
    // var x: int
    // x = 10
    // if (x > 5) {
    //   x = x + 5  // Block 1
    // } else {
    //   x = x - 5  // Block 2
    // }
    // x = x * 2    // Block 3 (post-branch)
    const ast = new ProgramNode([
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
    // var x: int
    // x = 0
    // while (x < 10) {
    //   if (x == 5) {
    //     return
    //   }
    //   x = x + 1    // Block 1 (inside loop)
    // }              // Block 2 (after loop)
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
    // var a: int, b: int
    // a = 10
    // b = 5
    // if (a > 0 && b < 10) {
    //   a = a + b  // Block 1
    // }
    const ast = new ProgramNode([
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
    // var x: int, y: int
    // x = 0
    // y = 10
    // while (x < 10) {
    //   if (x > 5 && y < 15) {
    //     y = y + 1    // Block 1
    //   }
    //   x = x + 1      // Block 2
    // }
    const ast = new ProgramNode([
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
    // var x: int
    // x = 1
    // foo(x, x > 0 && x < 10)
    const ast = new ProgramNode([

      new FunctionDeclarationNode(
        'foo',
        [
          new FunctionParameterNode('z', 'int', Capability.Let),
          new FunctionParameterNode('w', 'bool', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('x', true, 'int',
        new LiteralNode(1)
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
    // var point = Point{2, 3};
    // modifyIfPositive(point, 3);
    
    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'modifyIfPositive',
        [
          new FunctionParameterNode('p', 'Point', Capability.Inout), 
          new FunctionParameterNode('x', 'int', Capability.Let),
        ],
        'void',
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

  it('testStructFields', () => {
    // AST representing:
    // var point = Point{2, 3};
    // let x = point.x
    // let y = point.y
    // let z = x + y
    // point.x = z
    
    const ast = new ProgramNode([
      new VariableDeclarationNode('point', true, 'Point'),
      new VariableDeclarationNode('x', true, 'int'),
      new VariableDeclarationNode('y', true, 'int'),
      new VariableDeclarationNode('z', true, 'int'),
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
        new AssignmentNode(
          new IdentifierNode('x'),
          new MemberExpressionNode(new IdentifierNode('point'), 'Point', 'x')
        )
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('y'),
          new MemberExpressionNode(new IdentifierNode('point'), 'Point', 'y')
        )
      ),
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
      new ExpressionStatementNode(
        new AssignmentNode(
          new MemberExpressionNode(new IdentifierNode('point'), 'Point', 'x'),
          new IdentifierNode('z')
        )
      )
      
    ]);
    runTest("testStructFields", ast);
  })

  it('testInvalidBorrow', () => {
    // AST representing:
    // var point = Point{2, 3};
    // let x = point.x
    // point.x = 2
    // use(x)
    
    const ast = new ProgramNode([
      new VariableDeclarationNode('point', true, 'Point',
        new CreateStructNode(
          'Point',
          [new LiteralNode(2), new LiteralNode(3)]
        )
      ),
      new VariableDeclarationNode('x', false, 'int',
        new MemberExpressionNode(new IdentifierNode('point'), 'Point', 'x')
      ),
      
      new ExpressionStatementNode(
        new AssignmentNode(
          new MemberExpressionNode(new IdentifierNode('point'), 'Point', 'x'),
          new LiteralNode(2)
        )
      ),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('p', 'int', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'use',
          [new IdentifierNode('x')]
        )
      )
      
    ]);
    expect(() => runTest("testInvalidBorrow", ast)).toThrow("Cannot access")
  })

  it('testImmutableStruct', () => {
    // AST representing:
    // let point = Point{2, 3};
    // point.x = 5;
    
    const ast = new ProgramNode([
      new VariableDeclarationNode('point', false, 'Point',
        new CreateStructNode(
          'Point',
          [new LiteralNode(2), new LiteralNode(3)]
        )
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new MemberExpressionNode(new IdentifierNode('point'), 'Point', 'x'),
          new LiteralNode(5)
        )
      )
    ]);
    expect(() => runTest("testImmutableStruct", ast)).toThrow("Cannot assign")
  })

  it('testSink', () => {
    // AST representing:
    // let point = Point{2, 3};
    // var point2 = point
    // use(point)
    
    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('p', 'Point', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('point', false, 'Point', 
        new CreateStructNode(
          'Point',
          [new LiteralNode(2), new LiteralNode(3)]
        )
      ),
      new VariableDeclarationNode('point2', true, 'Point'),
      new ExpressionStatementNode(
        new AssignmentNode(
          new IdentifierNode('point2'),
          new IdentifierNode('point')
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'use',
          [new IdentifierNode('point')]
        )
      )
    ]);
    expect(() => runTest("testSink", ast)).toThrow("not definitely initialized");
  })

  it('testStructMultipleCalls', () => {
    // AST representing:
    // function addX(p: inout Point, x: int) { p.x = p.x + x; }
    // function addY(p: inout Point, y: int) { p.y = p.y + y; }
    // var result = Point{2, 3};
    // addX(result, 3);
    // addY(result, 2);

    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'addX',
        [
          new FunctionParameterNode('p', 'Point', Capability.Inout), 
          new FunctionParameterNode('x', 'int', Capability.Let),
        ],
        'void',
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
          new FunctionParameterNode('p', 'Point', Capability.Inout), 
          new FunctionParameterNode('y', 'int', Capability.Let),
        ],
        'void',
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

  it('testNestedStruct', () => {
    // AST representing:
    // function modify(l: inout Line, x: int) {
    //   l.p1.x = l.p1.x + x;
    //   l.p2.x = l.p2.x + x;
    // }
    // var result = Line{Point{2, 3}, Point{3, 4}};
    // modify(result, 3);

    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'modify',
        [
          new FunctionParameterNode('l', 'Line', Capability.Inout), 
          new FunctionParameterNode('x', 'int', Capability.Let),
        ],
        'void',
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new MemberExpressionNode(new MemberExpressionNode(new IdentifierNode('l'), 'Line', 'p1'), 'Point', 'x'),
              new BinaryExpressionNode(
                '+',
                new MemberExpressionNode(new MemberExpressionNode(new IdentifierNode('l'), 'Line', 'p1'), 'Point', 'x'),
                new IdentifierNode('x')
              )
            )
          ),
          new ExpressionStatementNode(
            new AssignmentNode(
              new MemberExpressionNode(new MemberExpressionNode(new IdentifierNode('l'), 'Line', 'p2'), 'Point', 'x'),
              new BinaryExpressionNode(
                '+',
                new MemberExpressionNode(new MemberExpressionNode(new IdentifierNode('l'), 'Line', 'p2'), 'Point', 'x'),
                new IdentifierNode('x')
              )
            )
          )
        ])
      ),
      new VariableDeclarationNode('result', true, 'Line',
        new CreateStructNode(
          'Line',
          [
            new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)]),
            new CreateStructNode('Point', [new LiteralNode(3), new LiteralNode(4)]),
          ]
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'modify',
          [new IdentifierNode('result'), new LiteralNode(3)]
        )
      )
    ]);
    runTest("testNestedStruct", ast);
  })

  it('testNestedStructProject', () => {
    // AST representing:
    // function modify(l: inout Line) {
    // }
    // var result = Line{Point{2, 3}, Point{3, 4}};
    // let p1 = result.p1;
    // modify(result);
    // use(p1.x);

    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'modify',
        [
          new FunctionParameterNode('l', 'Line', Capability.Inout), 
        ],
        'void',
        new BlockStatementNode([])
      ),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('z', 'int', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('result', true, 'Line',
        new CreateStructNode(
          'Line',
          [
            new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)]),
            new CreateStructNode('Point', [new LiteralNode(3), new LiteralNode(4)]),
          ]
        )
      ),
      new VariableDeclarationNode('p1', false, 'Point',
        new MemberExpressionNode(new IdentifierNode('result'), 'Line', 'p1')
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'modify',
          [new IdentifierNode('result')]
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'use',
          [new MemberExpressionNode(new IdentifierNode('p1'), 'Point', 'x')]
        )
      )
    ]);
    expect(() => runTest("testNestedStructProject", ast)).toThrow("Cannot access");
  })

  it('testNestedStructProjectSeparate', () => {
    // AST representing:
    // function modify(p: inout Point) {
    // }
    // var result = Line{Point{2, 3}, Point{3, 4}};
    // let p1 = result.p1;
    // modify(result.p2);
    // use(p1.x);

    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'modify',
        [
          new FunctionParameterNode('p', 'Point', Capability.Inout), 
        ],
        'void',
        new BlockStatementNode([])
      ),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('z', 'int', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('result', true, 'Line',
        new CreateStructNode(
          'Line',
          [
            new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)]),
            new CreateStructNode('Point', [new LiteralNode(3), new LiteralNode(4)]),
          ]
        )
      ),
      new VariableDeclarationNode('p1', false, 'Point',
        new MemberExpressionNode(new IdentifierNode('result'), 'Line', 'p1')
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'modify',
          [new MemberExpressionNode(new IdentifierNode('result'), 'Line', 'p2')]
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'use',
          [new MemberExpressionNode(new IdentifierNode('p1'), 'Point', 'x')]
        )
      ),
      new ExpressionStatementNode(
        new BuiltinNode('print',
          new MemberExpressionNode(new IdentifierNode('p1'), 'Point', 'x')
        )
      )
    ]);
    runTest("testNestedStructProjectSeparate", ast);
  })

  it('testNestedStructSink', () => {
    // AST representing:
    // function modify(l: inout Line, x: int) {
    // }
    // var result = Line{Point{2, 3}, Point{3, 4}};
    // var p1 = result.p1;
    // modify(result, 3);
    // use(p1.x);

    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'modify',
        [
          new FunctionParameterNode('l', 'Line', Capability.Inout), 
          new FunctionParameterNode('x', 'int', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('z', 'int', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('result', true, 'Line',
        new CreateStructNode(
          'Line',
          [
            new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)]),
            new CreateStructNode('Point', [new LiteralNode(3), new LiteralNode(4)]),
          ]
        )
      ),
      new VariableDeclarationNode('p1', true, 'Point',
        new MemberExpressionNode(new IdentifierNode('result'), 'Line', 'p1')
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'modify',
          [new IdentifierNode('result'), new LiteralNode(3)]
        )
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'use',
          [new MemberExpressionNode(new IdentifierNode('p1'), 'Point', 'x')]
        )
      )
    ]);
    expect(() => runTest("testNestedStructSink", ast)).toThrow("not definitely initialized");
  })

  it('testMutateBorrowedField', () => {
    // AST representing:
    // var result = Line{Point{2, 3}, Point{3, 4}};
    // let p1 = result.p1;
    // result.p1.x = 5;
    // use(p1.x);

    const ast = new ProgramNode([
      new VariableDeclarationNode('result', true, 'Line',
        new CreateStructNode(
          'Line',
          [
            new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)]),
            new CreateStructNode('Point', [new LiteralNode(3), new LiteralNode(4)]),
          ]
        )
      ),
      new VariableDeclarationNode('p1', false, 'Point',
        new MemberExpressionNode(new IdentifierNode('result'), 'Point', 'p1')
      ),
      new ExpressionStatementNode(
        new AssignmentNode(
          new MemberExpressionNode(
            new MemberExpressionNode(new IdentifierNode('result'), 'Line', 'p1'),
            'Point', 'x'),
          new LiteralNode(5)
        )
      ),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('z', 'int', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode(
          'use',
          [new MemberExpressionNode(new IdentifierNode('p1'), 'Point', 'x')]
        )
      )
    ]);
    expect(() => runTest("testMutateBorrowedField", ast)).toThrow("Cannot access");
  });

  it('testMutableLetArgument', () => {
    // AST representing:
    // var p1 = Point{2, 3}
    // fn thing(p: let Point) {
    //   p.x = 5
    // }
    // thing(p1)

    const ast = new ProgramNode([
      new VariableDeclarationNode('p1', false, 'Point',
        new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)])
      ),
      new FunctionDeclarationNode(
        'thing',
        [
          new FunctionParameterNode('p', 'Point', Capability.Let),
        ],
        'void',
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'x'),
              new LiteralNode(5)
            )
          ),
        ])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode('thing', [new IdentifierNode('p1')])
      )
    ]);
    expect(() => runTest("testMutableLetArgument", ast)).toThrow("Cannot assign");
  });

  it('testInvalidBorrowParam', () => {
    // AST representing:
    // fn thing(p: inout Point) {
    // }
    // let p1 = Point{2, 3}
    // thing(p1)

    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'thing',
        [
          new FunctionParameterNode('p', 'Point', Capability.Inout),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('p1', false, 'Point',
        new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode('thing', [new IdentifierNode('p1')])
      )
    ]);
    expect(() => runTest("testInvalidBorrowParam", ast)).toThrow("Cannot access");
  });

  it('testInoutArgumentMoved', () => {
    // AST representing:
    // var p1 = Point{2, 3}
    // fn thing(p: inout Point) {
    //   p.x = 5
    //   var p1 = p
    // }
    // thing(p1)

    const ast = new ProgramNode([
      new VariableDeclarationNode('p1', true, 'Point',
        new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)])
      ),
      new FunctionDeclarationNode(
        'thing',
        [
          new FunctionParameterNode('p', 'Point', Capability.Inout),
        ],
        'void',
        new BlockStatementNode([
          new ExpressionStatementNode(
            new AssignmentNode(
              new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'x'),
              new LiteralNode(5)
            )
          ),
          new ExpressionStatementNode(
            new VariableDeclarationNode(
              'p1', true, 'Point', new IdentifierNode('p')
            )
          ),
        ])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode('thing', [new IdentifierNode('p1')])
      )
    ]);
    expect(() => runTest("testInoutArgumentMoved", ast)).toThrow("not definitely initialized");
  });

  it('testReturnPrimitive', () => {
    // AST representing:
    // var p1 = Point{2, 3}
    // fn thing(p: let Point) {
    //   p.x
    // }
    // var x = thing(p1)

    const ast = new ProgramNode([
      new VariableDeclarationNode('p1', true, 'Point',
        new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)])
      ),
      new FunctionDeclarationNode(
        'thing',
        [
          new FunctionParameterNode('p', 'Point', Capability.Let),
        ],
        'int',
        new BlockStatementNode([
          new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'x')
        ])
      ),
      new VariableDeclarationNode('x', true, 'int',
        new CallExpressionNode('thing', [new IdentifierNode('p1')])
      )
    ]);
    runTest("testReturnPrimitive", ast)
  });

  it('testCopy', () => {
    // AST representing:
    // fn thing(p: let Point) {
    //   p.x
    // }
    // fn use(p: Point) {
    // }
    // var p1 = Point{2, 3}
    // var p2 = p1.copy
    // thing(p1)
    // thing(p2)
    // use(p2)
    // use(p1)

    const ast = new ProgramNode([
      new FunctionDeclarationNode(
        'thing',
        [
          new FunctionParameterNode('p', 'Point', Capability.Let),
        ],
        'int',
        new BlockStatementNode([
          new MemberExpressionNode(new IdentifierNode('p'), 'Point', 'x')
        ])
      ),
      new FunctionDeclarationNode(
        'use',
        [
          new FunctionParameterNode('p', 'Point', Capability.Let),
        ],
        'void',
        new BlockStatementNode([])
      ),
      new VariableDeclarationNode('p1', true, 'Point',
        new CreateStructNode('Point', [new LiteralNode(2), new LiteralNode(3)])
      ),
      new VariableDeclarationNode('p2', true, 'Point',
        new BuiltinNode('copy', new IdentifierNode('p1'))
      ),
      new ExpressionStatementNode(
        new CallExpressionNode('thing', [new IdentifierNode('p1')])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode('thing', [new IdentifierNode('p2')])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode('use', [new IdentifierNode('p2')])
      ),
      new ExpressionStatementNode(
        new CallExpressionNode('use', [new IdentifierNode('p1')])
      )
    ]);
    runTest("testCopy", ast)
  });
})