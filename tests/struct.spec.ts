import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("Struct type args", () => {

  const input = (`


fn thing4(x: int):
  return x + 2

fn fam!(T)(a: int):
  print(a)
  print(T)

fn famz!(T)(a: T):
  print(a)

struct Thing:
  x : int
  y : float

struct List!(T):
  elem : T

fn foothing(a: Thing):
  print(a.x)
  print(a)

fn foo_list!(E)(a: List!E):
  print(a.elem)

fn main():

  foo :: {|x| x + 1}

  foo(1)

  my_list: List!int
  my_list.elem = 32
  foo_list!int(my_list)

  thing: Thing
  print(thing.x + 32)
  foothing(thing)

  fam!2(1)
  fam!(1 + 1)(2)

  famz!int(200)
  famz!bool(true)


`)
  runCompilerTest(input)
  
});
