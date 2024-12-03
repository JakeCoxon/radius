import { textColors } from "../src/defs";

export type Regions = Region[];

export class IrFunction {
  regions: Regions = []
  instructions: IrInstruction[] = []
  root: number
}

export class BlockRegion {
  instructions: InstructionId[] = []
  parentRegion: number
  result: string
  constructor() {
  }
}

export class SequenceRegion {
  regions: number[] = []
  parentRegion: number
  result: string
  constructor() {
  }
}

export class WhileRegion {
  parentRegion: number
  condition: number
  bodyRegion: number
  result: string
  constructor() {}
}

export class IfRegion {
  parentRegion: number
  condition: number
  thenRegion: number
  elseRegion: number
  result: string
  constructor() {}
}

export type Region = BlockRegion | SequenceRegion | IfRegion | WhileRegion;

export class InstructionId {
  constructor(public regionId: number, public instructionId: number) {}
}
export class IrInstruction {
  constructor(public result: string, public label: string, public operands: string[]) {}
}


// const traverseSequenceRegion = (region: SequenceRegion, depth = 0) => {
//   region.regions.forEach((innerRegion, index) => {
//     const inden = " ".repeat(depth * 2)
//     console.log(inden, "Region", index)
//     visitRegion(innerRegion, depth);
//   });
// }
// const visitRegion = (region: Region, depth = 0) => {
//   const inden = " ".repeat(depth * 2)
//   if (region instanceof BlockRegion) {
//     region.instructions.forEach(instruction => {
//       console.log(inden, instruction.label);
//     });
//   } else if (region instanceof SequenceRegion) {
//     console.log(inden, "Sequence", region.result)
//     traverseSequenceRegion(region, depth + 1);
//   } else if (region instanceof IfRegion) {
//     // console.log(inden, "If", region.condition)
//     visitRegion(region.condition, depth + 1);
//     // console.log(inden, "Then", region.thenRegion)
//     if (region.thenRegion !== null) visitRegion(region.thenRegion, depth + 1);
//     // console.log(inden, "Else", region.elseRegion)
//     if (region.elseRegion !== null) visitRegion(region.elseRegion, depth + 1);
//   }
// }

export  const printFunction = (function_: IrFunction) => {
  const visitRegion = (region: Region, label: string, depth = 0, isLast = true, prefix = "") => {
    const branch = isLast ? "└─ " : "├─ ";
    const connection = depth > 0 ? prefix + branch : "";
    const typeLabel = region ? textColors.blue(`[${region.constructor.name.replace("Region", "")}]`) : textColors.gray("undefined");
    console.log(`${connection}${textColors.green(label)}`, typeLabel, region?.result ?? "");
    
    let nextPrefix = prefix + (depth === 0 ? '' : isLast ? "   " : "│  ");
    
    if (region instanceof BlockRegion) {
      region.instructions.forEach((instrId, index) => {
        const instruction = function_.instructions[instrId.instructionId];
        const isLastInstruction = index === region.instructions.length - 1;
        let label = instruction.result ? `${instruction.result} = ${instruction.label}` : instruction.label;
        label = label.startsWith("//") ? textColors.gray(label) : label;
        let indent = nextPrefix + (isLastInstruction ? "   " : "   ")
        indent += (" ".repeat(Math.max(0, 15 - indent.length)));
        console.log(indent + label, instruction.operands.join(' '));
      });
    } else if (region instanceof SequenceRegion) {
      region.regions.forEach((index, i) => {
        const innerRegion = function_.regions[index];
        visitRegion(innerRegion, `Region ${i}`, depth + 1, i === region.regions.length - 1, nextPrefix);
      });
    } else if (region instanceof IfRegion) {
      visitRegion(function_.regions[region.condition], "Cond", depth + 1, false, nextPrefix);
      if (region.thenRegion !== null) {
        const isLastBranch = region.elseRegion === null;
        visitRegion(function_.regions[region.thenRegion], "Then", depth + 1, isLastBranch, nextPrefix);
      }
      if (region.elseRegion !== null) {
        visitRegion(function_.regions[region.elseRegion], "Else", depth + 1, true, nextPrefix);
      }
    } else if (region instanceof WhileRegion) {
      visitRegion(function_.regions[region.condition], "Cond", depth + 1, false, nextPrefix);
      visitRegion(function_.regions[region.bodyRegion], "Body", depth + 1, true, nextPrefix);
    }
  };

  visitRegion(function_.regions[function_.root], "Root");
};

export class Codegen {
  function: IrFunction
  constructor(function_: IrFunction) {
    this.function = function_;
  }

  insertNewSequenceRegion(): number {
    const region = new SequenceRegion()
    this.function.regions.push(region);
    return this.function.regions.length - 1;
  }

  insertSequenceChild(parent: number, child: number) {
    const region = this.function.regions[parent] as SequenceRegion;
    const childRegion = this.function.regions[child];
    region.regions.push(child);
    childRegion.parentRegion = parent;
  }

  insertNewBlockRegion(): number {
    const region = new BlockRegion()
    this.function.regions.push(region);
    return this.function.regions.length - 1;
  }

  insertBlockInstruction(parent: number, instruction: IrInstruction) {
    const region = this.function.regions[parent] as BlockRegion;
    const instrId = new InstructionId(parent, this.function.instructions.length);
    region.instructions.push(instrId);
    this.function.instructions.push(instruction);
    return instrId;
  }

  insertNewIfRegion(): number {
    const region = new IfRegion()
    this.function.regions.push(region);
    return this.function.regions.length - 1;
  }

  insertIfCond(parent: number, condRegion: number) {
    const region = this.function.regions[parent] as IfRegion;
    region.condition = condRegion;
    const then = this.function.regions[condRegion];
    then.parentRegion = parent;
  }

  insertIfThen(parent: number, thenRegion: number) {
    const region = this.function.regions[parent] as IfRegion;
    region.thenRegion = thenRegion;
    const then = this.function.regions[thenRegion];
    then.parentRegion = parent;
  }

  insertIfElse(parent: number, elseRegion: number) {
    const region = this.function.regions[parent] as IfRegion;
    region.elseRegion = elseRegion;
    const else_ = this.function.regions[elseRegion];
    else_.parentRegion = parent;
  }

  insertNewWhileRegion(): number {
    const region = new WhileRegion()
    this.function.regions.push(region);
    return this.function.regions.length - 1;
  }

  insertWhileCond(parent: number, condRegion: number) {
    const region = this.function.regions[parent] as WhileRegion;
    region.condition = condRegion;
    const cond = this.function.regions[condRegion];
    cond.parentRegion = parent;
  }

  insertWhileBody(parent: number, bodyRegion: number) {
    const region = this.function.regions[parent] as WhileRegion;
    region.bodyRegion = bodyRegion;
    const body = this.function.regions[bodyRegion];
    body.parentRegion = parent;
  }
}