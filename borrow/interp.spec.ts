import { AllocInstruction, AssignInstruction, BasicBlock, ConditionalJumpInstruction, JumpInstruction, LoadConstantInstruction, LoadFieldInstruction, StoreFieldInstruction } from "./defs";
import { AbstractInterpreterIR } from "./interp";

// Helper function to assert conditions
function assert(condition: boolean, message: string) {
    if (!condition) {
        throw new Error(`Assertion failed: ${message}`);
    }
}

// Helper function to expect an error during interpretation
function expectError(fn: () => void) {
    try {
        fn();
        throw new Error('Expected an error but did not get one');
    } catch (error) {
        console.log(error.message);
    }
}

// Test 1: Uninitialized fields in aggregate types (existing test)
function testUninitialized1() {
  const blocks: BasicBlock[] = [
    new BasicBlock('entry', [
      new AllocInstruction('r_obj'),
      new ConditionalJumpInstruction(true, 'then'),
      new JumpInstruction('else'),
    ]),

    new BasicBlock('then', [
      new StoreFieldInstruction('r_obj', 'a', '10'),
      new JumpInstruction('after'),
    ]),

    new BasicBlock('else', [
      new StoreFieldInstruction('r_obj', 'b', '20'),
      new JumpInstruction('after'),
    ]),

    new BasicBlock('after', [
      new LoadInstruction('r_a', 'r_obj', 'a'),
      new LoadInstruction('r_b', 'r_obj', 'b'),
    ]),
  ];
  const interpreter = new AbstractInterpreterIR(blocks);

  expectError(() => interpreter.interpret());
  console.log('testUninitialized1 test passed.\n');
}

// Test 2: Using a variable before initialization
function testUseBeforeInitialization() {
  const blocks: BasicBlock[] = [
    new BasicBlock('entry', [
      new AssignInstruction('r_x', 'r_y'), // Attempt to assign from uninitialized 'r_y'
    ]),
  ];

  const interpreter = new AbstractInterpreterIR(blocks);

  expectError(() => interpreter.interpret());
  console.log('testUseBeforeInitialization test passed.\n');
}

// Test 3: Variable initialized in one branch and used after merge
function testBranchInitialization() {
  const blocks: BasicBlock[] = [
    new BasicBlock('entry', [
      new ConditionalJumpInstruction(true, 'then'), // Simplified condition
      new JumpInstruction('else'),
    ]),

    new BasicBlock('then', [
      new LoadConstantInstruction('r_x', 42), // Initialize r_x
      new JumpInstruction('after'),
    ]),

    new BasicBlock('else', [
      // r_x not initialized here
      new JumpInstruction('after'),
    ]),

    new BasicBlock('after', [
      new AssignInstruction('r_y', 'r_x'), // Use r_x
    ]),
  ];

  const interpreter = new AbstractInterpreterIR(blocks);

  expectError(() => interpreter.interpret());
  console.log('testBranchInitialization test passed.\n');
}

// Test 4: Variable initialized inside a loop that may not execute
function testLoopInitialization() {
  const blocks: BasicBlock[] = [
    new BasicBlock('entry', [
      new JumpInstruction('loop_condition'),
    ]),

    new BasicBlock('loop_condition', [
      new ConditionalJumpInstruction(false, 'loop_body'), // Loop doesn't execute
      new JumpInstruction('after_loop'),
    ]),

    new BasicBlock('loop_body', [
      new LoadConstantInstruction('r_counter', 0),
      new JumpInstruction('loop_condition'),
    ]),

    new BasicBlock('after_loop', [
      new AssignInstruction('r_result', 'r_counter'), // Use r_counter after loop
    ]),
  ];

  const interpreter = new AbstractInterpreterIR(blocks);

  expectError(() => interpreter.interpret());
  console.log('testLoopInitialization test passed.\n');
}

// Test 5: Nested conditionals affecting variable initialization
function testNestedConditionals() {
  const blocks: BasicBlock[] = [
    new BasicBlock('entry', [
      new ConditionalJumpInstruction(true, 'outer_then'),
      new JumpInstruction('outer_else'),
    ]),

    new BasicBlock('outer_then', [
      new ConditionalJumpInstruction(false, 'inner_then'),
      new JumpInstruction('inner_else'),
    ]),

    new BasicBlock('inner_then', [
      new LoadConstantInstruction('r_x', 1), // Initialize r_x
      new JumpInstruction('after'),
    ]),

    new BasicBlock('inner_else', [
      // r_x not initialized here
      new JumpInstruction('after'),
    ]),

    new BasicBlock('outer_else', [
      // r_x not initialized here
      new JumpInstruction('after'),
    ]),

    new BasicBlock('after', [
      new AssignInstruction('r_y', 'r_x'), // Use r_x
    ]),
  ];

  const interpreter = new AbstractInterpreterIR(blocks);

  expectError(() => interpreter.interpret());
  console.log('testNestedConditionals test passed.\n');
}

// Test 6: Partially initialized aggregates with uninitialized fields
function testNestedFields() {
  const blocks: BasicBlock[] = [
    new BasicBlock('entry', [
      new AllocInstruction('r_obj'),
      new StoreFieldInstruction('r_obj', 'a', '10'), // Initialize obj.a
      new LoadInstruction('r_x', 'r_obj', 'a'), // Use obj.a (initialized)
      new LoadInstruction('r_y', 'r_obj', 'b'), // Use obj.b (uninitialized)
    ]),
  ];

  const interpreter = new AbstractInterpreterIR(blocks);

  expectError(() => interpreter.interpret());
  console.log('testNestedFields test passed.\n');
}

// Test 7: Re-initialization of variables
function testReInitialization() {
  const blocks: BasicBlock[] = [
    new BasicBlock('entry', [
      new LoadConstantInstruction('r_x', 10), // Initialize r_x
      new LoadConstantInstruction('r_x', 20), // Re-initialize r_x
      new AssignInstruction('r_y', 'r_x'), // Use r_x
    ]),
  ];

  const interpreter = new AbstractInterpreterIR(blocks);

  interpreter.interpret(); // Should not throw an error
  console.log('testReInitialization test passed.\n');
}

// Function to run all tests
function runAllTests() {
  testUninitialized1();
  testUseBeforeInitialization();
  testBranchInitialization();
  testLoopInitialization();
  testNestedConditionals();
  testNestedFields();
  testReInitialization();
}

// Run all tests
runAllTests();