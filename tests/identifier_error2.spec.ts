import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("Identifier error2", () => {

  const input = (`

fn main():

  print(thing())

  fn thing():
    print("OK")

`)
  const test = runCompilerTest(input, { filename: 'identifier_error2', expectError: true })

});