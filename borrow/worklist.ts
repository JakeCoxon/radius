import { ControlFlowGraph } from "./controlflow";
import { BasicBlock } from "./defs";

export class Worklist {
  worklist: { block: BasicBlock }[] = []
  visited: Set<BasicBlock> = new Set();

  constructor(public cfg: ControlFlowGraph) {
    for (const block of this.cfg.blocks) {
      this.worklist.push({ block });
    }
  }

  hasVisited(block: BasicBlock) { return this.visited.has(block) }

  canVisitBlock(block: BasicBlock) {
    const dom = this.cfg.getImmediateDominator(block)
    if (!dom) return true
    if (!this.hasVisited(dom)) return false
    return this.cfg.predecessors.get(block)!.every(pred =>
      this.hasVisited(pred) || this.cfg.dominates(block, pred))
  }

  fixedPoint(visit: (block: BasicBlock) => void) {

    while (this.worklist.length > 0) {
      const { block } = this.worklist.shift()!;
      if (!this.canVisitBlock(block)) {
        this.worklist.push({ block });
        continue;
      }

      visit(block)

    }

  }

  shift() {
    return this.worklist.shift();
  }

  addWork(block: BasicBlock) {
    this.worklist.push({ block });
  }

}