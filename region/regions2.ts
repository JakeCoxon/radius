import { compilerAssert, ParseFunction, textColors } from "../src/defs";

import * as fs from "node:fs";
import { makeParser } from "../src/parser";
import { Codegen, IrFunction, IrInstruction, printFunction } from "./region_codegen";
import { BasicRegionCompiler } from "./basicRegionCompiler";
import { inspect } from "node:util";

let compiler = new BasicRegionCompiler();

const main = () => {
  const file = fs.readFileSync("../tests/fixtures/region.rad", "utf8");
  console.log(file)

  const parser = makeParser(file, "region.rad");

  console.log(parser.rootNode)

  parser.rootNode.exprs.forEach(expr => {
    if (expr instanceof ParseFunction) {
      compiler.parseFunction(expr.functionDecl)
      return
    }
    compilerAssert(false, "Unknown expression type", { expr })
  })
}

try {
  main()
} catch (e) {

  
  console.log("")
  console.error(e.message)
  console.error(e.stack)
  if (e.info) {
    Object.entries(e.info).forEach(([key, value]) => {
      console.log(`${key}:`)
      console.log(`${inspect(value, false, 3, true)}`)
    })
  }
}

console.log("")
console.log("Function:")
printFunction(compiler.irFunction)

// compiler.compile(parser.rootNode);



// const irFunction = new IrFunction();
// const c = new Codegen(irFunction)
// const root = c.insertNewSequenceRegion()
// irFunction.root = root
// const block = c.insertNewBlockRegion()
// c.insertSequenceChild(root, block)
// c.insertBlockInstruction(block, new IrInstruction("x", "add", [1, 2]))

// const block2 = c.insertNewBlockRegion()
// c.insertSequenceChild(root, block2)
// c.insertBlockInstruction(block2, new IrInstruction("y", "sub", [3, 4]))

// const block3 = c.insertNewIfRegion()
// c.insertSequenceChild(root, block3)

// const block4 = c.insertNewBlockRegion()
// c.insertIfThen(block3, block4)
// c.insertBlockInstruction(block4, new IrInstruction("a", "cmp", [5, 6]))


// const root = new SequenceRegion("x", [
//   new BlockRegion("y", [
//     new IrInstruction("x", "add", [1, 2]),
//     new IrInstruction("y", "sub", [3, 4]),
//   ]),
//   new IfRegion("x", 
//     new BlockRegion("a", [
//       new IrInstruction("a", "cmp", [5, 6]),
//     ]),
//     new BlockRegion("b", [
//       new IrInstruction("b", "add", [7, 8]),
//     ]),
//     new BlockRegion("c", [
//       new IrInstruction("c", "sub", [9, 10]),
//     ]),
//   ),
// ]);

// printFunction(irFunction);