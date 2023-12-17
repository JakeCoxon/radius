import { functionTemplateTypeCheckAndCompileDef, runTopLevel } from "./compiler";
import { Closure, DoubleType, ExternalFunction, FloatType, IntType, Scope, StringType, VoidType, compilerAssert, compilerState, createScope, expectMap, pushSubCompilerState } from "./defs";
import { makeParser } from "./parser"
import { Queue, stepQueue } from "./tasks";

const parser = makeParser(`
print("Hello", 3 + 2)

x := 32

for x in foo:
  print("lol")

comptime:
  lol as asd

while true:
  print("Foo" + 3)

if 3:
  print("OK")

if 3 >= 2:
  print("OK")
elif 3 < 2:
  print("NOOO")
else:
  print("No")

x := not y
x := meta 2
x := x ifx 2 else 2

foo.too(3, 2)

x := |asd| asd + 2

x := {|asd| asd + 2}

defn thing2(foo: int):
  x := [1,2,3 | asdasd | 323123123 |> thing]
x := [1,2,3 | asdasd | 323123123 |> thing]

defn thing(x: int):
  print(x + 32)

defn thing4(x: int):
  return x + 2

defn fam!(T)(a: int):
  print(a)
  print(T)

defn main():
  print(2 + 42 * thing(12))

  if 2 < 3:
    print("OK")
  elif 3 > 2:
    print("wow")
  elif 3 > 2:
    print("wow")
  elif 3 > 2:
    print("wow")
  else:
    print(3)

  z : int = 2
  x : int = z ifx 2 else 2

  while false or true:
    print("OK")
  
  if 3:
    print("asd")

  meta if thing4(2):
    print("k thing4")

  meta if true:
    print("this is meta if")

  foo :: {|x| x + 1}

  foo(1)
  fam!2(1)
  
  print(meta thing(12))

  #for x in thing:
  #  print(x)


foo[1:2:3]
foo[1:2]
foo[1]
foo.[1] /= 2

foo!(A, B)(a, b)

class Thing!(T):
  x: int
  y: int

`)


const rootScope: Scope = createScope({
  int: IntType,
  float: FloatType,
  double: DoubleType,
  void: VoidType,
  string: StringType,
  compfoo: { _function: (a, b) => 65 + a + b },
  bar: 123,
  print: new ExternalFunction('print', (...args) => {
    console.log("print called", ...args);
    return args[0];
  }),
});

pushSubCompilerState({ })

runTopLevel(parser.ast, rootScope)


console.log(Bun.inspect(parser.ast, { depth: 10, colors: true }))

const func: Closure = expectMap(rootScope, "main", "No main function found");

const queue = new Queue();
queue.enqueue(functionTemplateTypeCheckAndCompileDef.of({ func: func.func, args: [], typeArgs: [], parentScope: rootScope}))

for (let i = 0; i < 1000; i++) {
  if (queue.list.length === 0) break;
  stepQueue(queue);
}

compilerAssert(queue.list.length === 0, "Expected empty quue")
console.log(queue.list.length)

// console.log(queue.final)

// const compiledFunction = functionTemplateTypeCheckAndCompile(func.func, [], [], rootScope);
// // console.log(Bun.inspect(compiledFunction.body, { depth: 10, colors: true }));

compilerState.global.compiledFunctions.forEach((func) => {
  console.log(func.functionDefinition.debugName)
  console.log(Bun.inspect(func.body, { depth: 10, colors: true }));
  console.log("")
})