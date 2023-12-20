import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("closure", () => {

  const input = (`

fn main():

  x := 32

  fn thing(x: int):
    52 + x

  print(thing(x))


`)
  const test = runCompilerTest(input, { filename: 'closure' })

});