import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test.todo("noshadow", () => {

  const input = (`


fn thing(x: int):
  32 + x

fn main():
  
  # should this reference outer scope or inner scope?

  foo :: thing(100)

  meta if 1 == 1:

    fn thing(x: int):
      100

  bar :: thing(100)

  comptime:
    print(foo)
    print(bar)


`)
  const test = runCompilerTest(input, { filename: "noshadow" })

  // These should be the same
  expect(test.prints).toEqual([
    100,
    100
  ])

  
});
