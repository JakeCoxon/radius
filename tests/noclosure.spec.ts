import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("noclosure", () => {

  const input = (`

fn main():

  x := 32

  fn thing():
    print(x)
    52

  print(thing())


`)
  const test = runCompilerTest(input, { filename: 'noclosure', expectError: true })

});