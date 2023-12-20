import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("list_iterator", () => {

  const input = (`

fn iterate!(f, T)(lst: List!T) @inline: 
  i := 0
  while i < lst.length:
    f(lst[i])
    i += 1

fn main():
  lst := [1,2,3]
  print(lst.length)
  print(lst[0])
  
  i := 0
  while i < lst.length:
    print(lst[i])
    i += 1

  f :: |x| print(x)
  iterate!(f, int)(lst)
  iterate!(f, int)(lst)
  iterate!(f, int)(lst)

`)
  const test = runCompilerTest(input, { filename: 'list_iterator' })

});
