import { functionTemplateTypeCheckAndCompile, runTopLevel } from "./compiler";
import { Closure, DoubleType, ExternalFunction, FloatType, IntType, Scope, StringType, VoidType, compilerState, createScope, expectMap, pushSubCompilerState } from "./defs";
import { makeParser } from "./parser"

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

defn fam!(T)(a: int):
  print(a)
  print(T)

defn main():
  print(2 + 42 * thing(12))

  if 2 < 3:
    print("OK")
  else:
    print(3)

  z : int = 2
  x : int = z ifx 2 else 2

  while false or true:
    print("OK")

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
const compiledFunction = functionTemplateTypeCheckAndCompile(func.func, [], [], rootScope);
// console.log(Bun.inspect(compiledFunction.body, { depth: 10, colors: true }));

compilerState.global.compiledFunctions.forEach((func) => {
  console.log(func.functionDefinition.debugName)
  console.log(Bun.inspect(func.body, { depth: 10, colors: true }));
  console.log("")
})