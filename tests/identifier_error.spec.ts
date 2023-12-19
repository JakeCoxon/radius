import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("Identifier error", () => {

  const input = (`

foo :: thing()

fn main():

  print(1)

`)
  const test = runCompilerTest(input, { filename: 'identifier_error', expectError: true })

});
