import { BasicBlock, ConditionalJumpInstruction, JumpInstruction } from "./defs";

export class ControlFlowGraph {
  blocks: BasicBlock[] = [];
  entry: BasicBlock;

  // State maps
  predecessors: Map<BasicBlock, BasicBlock[]> = new Map();
  successors: Map<BasicBlock, BasicBlock[]> = new Map();
  dom: Map<BasicBlock, BasicBlock | null> = new Map(); // Immediate dominators
  rpoNumber: Map<BasicBlock, number> = new Map(); // Reverse post-order numbers
  children: Map<BasicBlock, BasicBlock[]> = new Map(); // Children in the dominator tree

  constructor(entry: BasicBlock) {
    this.entry = entry;
    this.blocks.push(entry);

    // Initialize maps for the entry block
    this.predecessors.set(entry, []);
    this.successors.set(entry, []);
  }

  addBlock(block: BasicBlock) {
    this.blocks.push(block);
    this.predecessors.set(block, []);
    this.successors.set(block, []);
  }

  addEdge(from: BasicBlock, to: BasicBlock) {
    if (!this.successors.has(from)) {
      this.successors.set(from, []);
    }
    if (!this.predecessors.has(to)) {
      this.predecessors.set(to, []);
    }
    this.successors.get(from)!.push(to);
    this.predecessors.get(to)!.push(from);
  }

  computeReversePostOrder() {
    const visited = new Set<BasicBlock>();
    const rpo: BasicBlock[] = [];

    const dfs = (block: BasicBlock) => {
      if (visited.has(block)) return;
      visited.add(block);
      const successors = this.successors.get(block) || [];
      for (const succ of successors) {
        dfs(succ);
      }
      rpo.push(block);
    };

    dfs(this.entry);

    // Assign reverse post-order numbers
    const n = rpo.length;
    for (let i = 0; i < n; i++) {
      this.rpoNumber.set(rpo[i], n - i);
    }

    return rpo.reverse();
  }

  computeDominatorTree() {
    const rpo = this.computeReversePostOrder();

    // Initialize immediate dominators
    for (const block of this.blocks) {
      this.dom.set(block, null);
      this.children.set(block, []);
    }
    this.dom.set(this.entry, this.entry); // Entry node dominates itself

    let changed = true;
    while (changed) {
      changed = false;

      // Process nodes in reverse post-order, skipping the entry
      for (const block of rpo) {
        if (block === this.entry) continue;

        let newIdom: BasicBlock | null = null;

        // Find first processed predecessor
        const preds = this.predecessors.get(block) || [];
        for (const pred of preds) {
          if (this.dom.get(pred) !== null) {
            newIdom = pred;
            break;
          }
        }

        // Intersect dominators of all predecessors
        if (newIdom !== null) {
          for (const pred of preds) {
            if (pred !== newIdom && this.dom.get(pred) !== null) {
              newIdom = this.intersect(pred, newIdom);
            }
          }
        }

        if (this.dom.get(block) !== newIdom) {
          this.dom.set(block, newIdom);
          changed = true;
        }
      }
    }

    // Build the dominator tree
    for (const block of this.blocks) {
      if (block !== this.entry) {
        const idom = this.dom.get(block);
        if (idom != null) {
          this.children.get(idom)!.push(block);
        }
      }
    }
  }

  intersect(b1: BasicBlock, b2: BasicBlock): BasicBlock {
    let finger1 = b1;
    let finger2 = b2;

    while (finger1 !== finger2) {
      while (this.rpoNumber.get(finger1)! > this.rpoNumber.get(finger2)!) {
        finger1 = this.dom.get(finger1)!;
      }
      while (this.rpoNumber.get(finger2)! > this.rpoNumber.get(finger1)!) {
        finger2 = this.dom.get(finger2)!;
      }
    }
    return finger1;
  }

  printDominatorTree() {
    const printTree = (block: BasicBlock, indent: string) => {
      console.log(`${indent}Block ${block.label}`);
      const children = this.children.get(block);
      if (children) {
        for (const child of children) {
          printTree(child, indent + '  ');
        }
      }
    };
    printTree(this.entry, '');
  }

  /**
   * Checks if block `a` dominates block `b`.
   */
  dominates(a: BasicBlock, b: BasicBlock): boolean {
    let current = b;
    while (current !== null && current !== a) {
      current = this.dom.get(current)!;
      if (current === null || (current === this.entry && a !== this.entry)) return false;
    }
    return current === a;
  }

  /**
   * Checks if block `a` strictly dominates block `b`.
   */
  strictlyDominates(a: BasicBlock, b: BasicBlock): boolean {
    return a !== b && this.dominates(a, b);
  }

  /**
   * Checks if block `a` immediately dominates block `b`.
   */
  immediatelyDominates(a: BasicBlock, b: BasicBlock): boolean {
    return this.dom.get(b) === a;
  }

  /**
   * Retrieves the immediate dominator of a block.
   */
  getImmediateDominator(block: BasicBlock): BasicBlock | null {
    return this.dom.get(block) || null;
  }
}

export const buildCFG = (blocks: BasicBlock[]): ControlFlowGraph => {
  const entryBlock = blocks[0] // always first block
  if (!entryBlock) {
    throw new Error("Entry block not found.");
  }

  const cfg = new ControlFlowGraph(entryBlock);

  // Add blocks to CFG
  for (const block of blocks) {
    if (block !== entryBlock) {
      cfg.addBlock(block);
    }
  }

  // Add edges based on control flow instructions
  for (const block of blocks) {
    const lastInstr = block.instructions[block.instructions.length - 1];

    if (lastInstr instanceof JumpInstruction) {
      const targetBlock = blocks.find((b) => b.label === lastInstr.target);
      if (targetBlock) {
        cfg.addEdge(block, targetBlock);
      }
    } else if (lastInstr instanceof ConditionalJumpInstruction) {
      const thenBlock = blocks.find((b) => b.label === lastInstr.target);
      const elseBlockIndex = blocks.indexOf(block) + 1;
      const elseBlock = blocks[elseBlockIndex];

      if (thenBlock) {
        cfg.addEdge(block, thenBlock);
      }
      if (elseBlock) {
        cfg.addEdge(block, elseBlock);
      }
    } else {
      // Default to the next block if no explicit jump
      const nextBlockIndex = blocks.indexOf(block) + 1;
      if (nextBlockIndex < blocks.length) {
        const nextBlock = blocks[nextBlockIndex];
        cfg.addEdge(block, nextBlock);
      }
    }
  }

  cfg.computeReversePostOrder();
  cfg.computeDominatorTree();

  return cfg;
}

export const printCFG = (cfg: ControlFlowGraph) => {
  // Print cfg as DOT format
  console.log('digraph G {');
  for (const block of cfg.blocks) {
    const successors = cfg.successors.get(block) || [];
    for (const succ of successors) {
      console.log(`  ${block.label} -> ${succ.label}`);
    }
  }
  console.log('}');
}

export const printDominators = (cfg: ControlFlowGraph) => {
  // as DOT format
  console.log('digraph G {');
  for (const [block, dom] of cfg.dom) {
    if (dom) {
      console.log(`  ${dom.label} -> ${block.label}`);
    }
  }
  console.log('}');
}