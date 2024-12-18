import { BlockRegion, IfRegion, IrFunction, RegionId, ScopeRegion, SequenceId, WhileRegion } from "../region/region_codegen";
import { compilerAssert } from "../src/defs";
import { BasicBlock, ConditionalJumpInstruction, JumpInstruction } from "./defs";

export type ControlFlowGraph = ControlFlowGraphGeneric<BasicBlock>;

export class ControlFlowGraphGeneric<T> {
  blocks: T[] = [];
  entry: T;

  // State maps
  predecessors: Map<T, T[]> = new Map();
  successors: Map<T, T[]> = new Map();
  dom: Map<T, T | null> = new Map(); // Immediate dominators
  rpoNumber: Map<T, number> = new Map(); // Reverse post-order numbers
  children: Map<T, T[]> = new Map(); // Children in the dominator tree

  constructor(entry: T) {
    this.entry = entry;
    this.blocks.push(entry);

    // Initialize maps for the entry block
    this.predecessors.set(entry, []);
    this.successors.set(entry, []);
  }

  addBlock(block: T) {
    this.blocks.push(block);
    this.predecessors.set(block, []);
    this.successors.set(block, []);
  }

  addEdge(from: T, to: T) {
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
    const visited = new Set<T>();
    const rpo: T[] = [];

    const dfs = (block: T) => {
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

  computeDominanceDepth(): Map<T, number> {
    const depthMap: Map<T, number> = new Map();

    const dfs = (block: T, currentDepth: number) => {
      depthMap.set(block, currentDepth);
      const children = this.children.get(block) || [];
      for (const child of children) {
        dfs(child, currentDepth + 1);
      }
    }

    dfs(this.entry, 0);
    return depthMap;
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

        let newIdom: T | null = null;

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

  intersect(b1: T, b2: T): T {
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

  printDominatorTree(print: (indent: string, block: T) => void) {
    const printTree = (block: T, indent: string) => {
      print(indent, block)
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
  dominates(a: T, b: T): boolean {
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
  strictlyDominates(a: T, b: T): boolean {
    return a !== b && this.dominates(a, b);
  }

  /**
   * Checks if block `a` immediately dominates block `b`.
   */
  immediatelyDominates(a: T, b: T): boolean {
    return this.dom.get(b) === a;
  }

  /**
   * Retrieves the immediate dominator of a block.
   */
  getImmediateDominator(block: T): T | null {
    return this.dom.get(block) || null;
  }
}

export const buildCFG = (blocks: BasicBlock[]): ControlFlowGraph => {
  const entryBlock = blocks[0] // always first block
  if (!entryBlock) {
    throw new Error("Entry block not found.");
  }

  const cfg = new ControlFlowGraphGeneric(entryBlock);

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
      compilerAssert(targetBlock, `Target block ${lastInstr.target} not found.`);
      cfg.addEdge(block, targetBlock);
    } else if (lastInstr instanceof ConditionalJumpInstruction) {
      const thenBlock = blocks.find((b) => b.label === lastInstr.targetLabel);
      const elseBlock = blocks.find((b) => b.label === lastInstr.elseLabel);

      compilerAssert(thenBlock, `Then block ${lastInstr.targetLabel} not found.`);
      compilerAssert(elseBlock, `Else block ${lastInstr.elseLabel} not found.`);
      cfg.addEdge(block, thenBlock);
      cfg.addEdge(block, elseBlock);
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

export const buildCFGFromRegions = (irFunction: IrFunction): ControlFlowGraphGeneric<RegionId> => {

  const regionIds = irFunction.regions.map((r, id) => id);
  const entryRegion = 0;

  const cfg = new ControlFlowGraphGeneric<RegionId>(entryRegion as RegionId);
  for (const regionId of regionIds) {
    if (regionId !== entryRegion && irFunction.regions[regionId] instanceof BlockRegion) {
      cfg.addBlock(regionId as RegionId);
    }
  }

  const firstBlockRegion = (sequenceId: SequenceId) => {
    const seq = irFunction.sequences[sequenceId];
    let regionId = seq.firstChildRegion;
    while (regionId !== null) {
      const region = irFunction.regions[regionId];
      if (region instanceof BlockRegion) return regionId;
      else if (region instanceof IfRegion) return firstBlockRegion(region.conditionSequence);
      else if (region instanceof WhileRegion) return firstBlockRegion(region.conditionSequence);
      else if (region instanceof ScopeRegion) return firstBlockRegion(region.bodySequence);
      else compilerAssert(false, "Unknown region type", { region })
    }
    compilerAssert(false, "No block region found in sequence", { sequenceId })
  }

  const visitSequence = (prevRegionId: RegionId | null, sequenceId: SequenceId): RegionId | null => {
    const seq = irFunction.sequences[sequenceId];
    for (let region = seq.firstChildRegion; region !== null; region = irFunction.regions[region].nextRegion) {
      
      prevRegionId = visitRegion(prevRegionId, region);
    }
    return prevRegionId;
  }
  const visitRegion = (prevRegionId: RegionId | null, regionId: RegionId): RegionId | null => {
    const region = irFunction.regions[regionId];
    if (region instanceof BlockRegion) {
      if (prevRegionId !== null) cfg.addEdge(prevRegionId, regionId);
      return regionId
      // compilerAssert(false, "Not implemented yet", { region, currentRegion, regionId })
    } else if (region instanceof IfRegion) {

      const first = firstBlockRegion(region.conditionSequence);
      compilerAssert(first, "No condition block found", { region })
      const exit = firstBlockRegion(region.exitSequence);
      
      const cond = visitSequence(prevRegionId, region.conditionSequence);
      const then = visitSequence(cond, region.thenSequence);
      const else_ = visitSequence(cond, region.elseSequence);
      
      compilerAssert(then, "No then block found", { region })
      compilerAssert(else_, "No else block found", { region })
      cfg.addEdge(then, exit);
      cfg.addEdge(else_, exit);
      return visitSequence(null, region.exitSequence);
    } else if (region instanceof WhileRegion) {

      const first = firstBlockRegion(region.conditionSequence);
      compilerAssert(first, "No condition block found", { region })
      const exit = firstBlockRegion(region.exitSequence);
      const cond = visitSequence(prevRegionId, region.conditionSequence);
      const body = visitSequence(cond, region.bodySequence);
      compilerAssert(cond, "No condition block found", { region })
      compilerAssert(body, "No body block found", { region })
      cfg.addEdge(body, cond);
      cfg.addEdge(cond, exit);
      return visitSequence(null, region.exitSequence);
    } else if (region instanceof ScopeRegion) {
      const first = firstBlockRegion(region.bodySequence);
      const exit = firstBlockRegion(region.exitSequence);
      compilerAssert(first, "No body block found", { region })
      compilerAssert(exit, "No exit block found", { region })
      const body = visitSequence(prevRegionId, region.bodySequence);
      compilerAssert(body, "No body block found", { region })
      cfg.addEdge(body, exit);
      return visitSequence(null, region.exitSequence);
    }
    compilerAssert(false, "Unknown region type", { region })
  }
  visitSequence(null, irFunction.root)

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
