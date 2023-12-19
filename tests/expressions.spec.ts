import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("Expressions", () => {

  const input = (`

fn foo():
  
  thing := 23 ifx 2 == 2 else 32
  thing2 := (23 ifx 2 == 2 else 32)

  thing = 2 if true
  print(23) if true

  thing3 := block:
    x := 32

    if 2 == 2:
      32
    elif 2 == 3:
      if true:
        #print("OK")
        3
      else:
        23123123
    else:
      32

    
fn main():
  meta foo()

`)
  const test = runCompilerTest(input, { filename: 'expressions' })

});
