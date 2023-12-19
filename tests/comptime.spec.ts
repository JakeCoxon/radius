import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("Comptime", () => {

  const input = (`

foo2 :: foo + 3
foo :: thing4(5)


fn thing(x: int):
  print("thing", x)
  print(x + 32)

fn thing4(x: int):
  print(x)
  return x + 2

fn foop(a: int, b: int):
  print(2 + 42 * thing(12))

  print(meta thing(12))

fn main():
  print(2 + 42 * thing(12))


  meta foop(2, 1)
  foop(2, 1)


`)
  const test = runCompilerTest(input)

  expect(test.prints).toEqual(
   [
     5,
     "\"thing\"",
     12,
     44,
     1850,
     "\"thing\"",
     12,
     44,
     44,
     "\"thing\"",
     12,
     44
   ]
  )
  
});
