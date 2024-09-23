// Test Cases for Edge Cases

import { ControlFlowGraph } from "./controlflow";
import { BasicBlock } from "./defs";

// Helper function to assert conditions
function assert(condition: boolean, message: string) {
    if (!condition) {
        throw new Error(`Assertion failed: ${message}`);
    }
}

// 1. Graph with Unreachable Nodes
function testUnreachableNodes() {
    console.log('Test 1: Graph with Unreachable Nodes');
    const entry = new BasicBlock("1", []);
    const b2 = new BasicBlock("2", []);
    const b3 = new BasicBlock("3", []); // Unreachable
    const b4 = new BasicBlock("4", []); // Unreachable

    const cfg = new ControlFlowGraph(entry);

    cfg.addBlock(b2);
    cfg.addBlock(b3);
    cfg.addBlock(b4);

    cfg.addEdge(entry, b2);

    cfg.computeDominatorTree();

    // Unreachable nodes should not be in the dominator tree
    cfg.printDominatorTree();

    // Assertions
    assert(cfg.dominates(entry, b2), 'Entry should dominate b2');
    assert(cfg.dom.get(b3) == null, 'b3 should not have a dominator');
    assert(cfg.dom.get(b4) == null, 'b4 should not have a dominator');

    assert(cfg.successors.get(entry)!.length === 1, 'Entry should have 1 successor');
    assert(cfg.successors.get(b2)!.length === 0, 'b2 should have 0 successors');
    assert(cfg.successors.get(b3)!.length === 0, 'b3 should have 0 successors');

    console.log('Test 1 passed.\n');
}

// 2. Graph with Multiple Loops
function testMultipleLoops() {
    console.log('Test 2: Graph with Multiple Loops');
    const entry = new BasicBlock("1", []);
    const b2 = new BasicBlock("2", []);
    const b3 = new BasicBlock("3", []);
    const b4 = new BasicBlock("4", []);
    const b5 = new BasicBlock("5", []);

    const cfg = new ControlFlowGraph(entry);

    cfg.addBlock(b2);
    cfg.addBlock(b3);
    cfg.addBlock(b4);
    cfg.addBlock(b5);

    // First loop
    cfg.addEdge(entry, b2);
    cfg.addEdge(b2, b3);
    cfg.addEdge(b3, b2); // Loop back to b2

    // Second loop
    cfg.addEdge(b3, b4);
    cfg.addEdge(b4, b5);
    cfg.addEdge(b5, b3); // Loop back to b3

    cfg.computeDominatorTree();

    cfg.printDominatorTree();

    // Assertions
    assert(cfg.dominates(entry, b2), 'Entry should dominate b2');
    assert(cfg.dominates(entry, b5), 'Entry should dominate b5');
    assert(cfg.dominates(b3, b5), 'b3 should dominate b5');
    assert(cfg.immediatelyDominates(b3, b4), 'b3 should immediately dominate b4');

    console.log('Test 2 passed.\n');
}

// 3. Graph with Converging Paths
function testConvergingPaths() {
    console.log('Test 3: Graph with Converging Paths');
    const entry = new BasicBlock("1", []);
    const b2 = new BasicBlock("2", []);
    const b3 = new BasicBlock("3", []);
    const b4 = new BasicBlock("4", []);
    const b5 = new BasicBlock("5", []);

    const cfg = new ControlFlowGraph(entry);

    cfg.addBlock(b2);
    cfg.addBlock(b3);
    cfg.addBlock(b4);
    cfg.addBlock(b5);

    cfg.addEdge(entry, b2);
    cfg.addEdge(entry, b3);
    cfg.addEdge(b2, b4);
    cfg.addEdge(b3, b4);
    cfg.addEdge(b4, b5);

    cfg.computeDominatorTree();

    cfg.printDominatorTree();

    // Assertions
    assert(cfg.dominates(entry, b4), 'Entry should dominate b4');
    assert(cfg.getImmediateDominator(b4) === entry, 'Entry should immediately dominate b4');
    assert(cfg.getImmediateDominator(b2) === entry, 'Entry should immediately dominate b2');
    assert(cfg.getImmediateDominator(b3) === entry, 'Entry should immediately dominate b3');

    console.log('Test 3 passed.\n');
}

// 4. Graph with Cycles and Back Edges
function testCyclesAndBackEdges() {
    console.log('Test 4: Graph with Cycles and Back Edges');
    const entry = new BasicBlock("1", []);
    const b2 = new BasicBlock("2", []);
    const b3 = new BasicBlock("3", []);

    const cfg = new ControlFlowGraph(entry);

    cfg.addBlock(b2);
    cfg.addBlock(b3);

    cfg.addEdge(entry, b2);
    cfg.addEdge(b2, b3);
    cfg.addEdge(b3, b2); // Back edge creating a cycle

    cfg.computeDominatorTree();

    cfg.printDominatorTree();

    // Assertions
    assert(cfg.dominates(entry, b3), 'Entry should dominate b3');
    assert(cfg.getImmediateDominator(b2) === entry, 'Entry should immediately dominate b2');
    assert(cfg.getImmediateDominator(b3) === b2, 'b2 should immediately dominate b3');

    console.log('Test 4 passed.\n');
}

// 5. Graph with a Single Node (Entry Node Only)
function testSingleNode() {
    console.log('Test 5: Graph with a Single Node (Entry Node Only)');
    const entry = new BasicBlock("1", []);

    const cfg = new ControlFlowGraph(entry);

    cfg.computeDominatorTree();

    cfg.printDominatorTree();

    // Assertions
    assert(cfg.getImmediateDominator(entry) === entry, 'Entry should dominate itself');

    console.log('Test 5 passed.\n');
}

// 6. Disconnected Graph
function testDisconnectedGraph() {
    console.log('Test 6: Disconnected Graph');
    const entry = new BasicBlock("1", []);
    const b2 = new BasicBlock("2", []); // Disconnected
    const b3 = new BasicBlock("3", []); // Disconnected

    const cfg = new ControlFlowGraph(entry);

    cfg.addBlock(b2);
    cfg.addBlock(b3);

    cfg.computeDominatorTree();

    cfg.printDominatorTree();

    // Assertions
    assert(cfg.getImmediateDominator(entry) === entry, 'Entry should dominate itself');
    assert(cfg.dom.get(b2) == null, 'b2 should not have a dominator');
    assert(cfg.dom.get(b3) == null, 'b3 should not have a dominator');

    console.log('Test 6 passed.\n');
}

// 7. Graph with Multiple Entry Points (Invalid CFG)
function testMultipleEntryPoints() {
    console.log('Test 7: Graph with Multiple Entry Points (Invalid CFG)');
    const entry = new BasicBlock("1", []);
    const b2 = new BasicBlock("2", []); // Another entry point

    const cfg = new ControlFlowGraph(entry);

    cfg.addBlock(b2);

    // No edges added, b2 is disconnected and acts as another entry point

    cfg.computeDominatorTree();

    cfg.printDominatorTree();

    // Assertions
    assert(cfg.getImmediateDominator(entry) === entry, 'Entry should dominate itself');
    assert(cfg.dom.get(b2) == null, 'b2 should not have a dominator');

    console.log('Test 7 passed.\n');
}

// 8. Graph with Self-Loops
function testSelfLoops() {
    console.log('Test 8: Graph with Self-Loops');
    const entry = new BasicBlock("1", []);
    const b2 = new BasicBlock("2", []);

    const cfg = new ControlFlowGraph(entry);

    cfg.addBlock(b2);

    cfg.addEdge(entry, b2);
    cfg.addEdge(b2, b2); // Self-loop

    cfg.computeDominatorTree();

    cfg.printDominatorTree();

    // Assertions
    assert(cfg.getImmediateDominator(b2) === entry, 'Entry should immediately dominate b2');
    assert(cfg.dominates(b2, b2), 'b2 should dominate itself');

    console.log('Test 8 passed.\n');
}

// Run all tests
function runAllTests() {
    testUnreachableNodes();
    testMultipleLoops();
    testConvergingPaths();
    testCyclesAndBackEdges();
    testSingleNode();
    testDisconnectedGraph();
    testMultipleEntryPoints();
    testSelfLoops();
}

runAllTests();