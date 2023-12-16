import { functionTemplateTypeCheckAndCompile } from "./compiler";
import { Closure, DoubleType, ExternalFunction, FloatType, FunctionDefinition, IntType, ParseStatements, Scope, StringType, VoidType, addFunctionDefinition, compilerState, createScope, expect, expectMap, pushSubCompilerState } from "./defs";
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

x := {
  defn foo(x):
    print(2)
  
  defn thing(x):
    print(x)
}

x := {|asd| asd + 2}

defn thing2(foo: int):
  x := [1,2,3 | asdasd | 323123123 |> thing]
x := [1,2,3 | asdasd | 323123123 |> thing]

defn thing(x: int):
  print(x + 32)

defn main():
  print(2 + 42 * thing(12))

  foo :: {|x| x + 1}

  foo(1)
  
  print(meta thing(12))


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

parser.ast.exprs.forEach(expr => {
  if (expr.key === 'function') {

    const funcDef = addFunctionDefinition(expr.functionDecl)
    // const func = parser.functionDecls[expr.id]
    console.log("Global func", funcDef.name)

    
    rootScope[funcDef.name!.token.value] = new Closure(funcDef, rootScope);
    

    return

  }
  // throw new Error(`Not supported at top level ${expr.key}`)
})

console.log(Bun.inspect(parser.ast, { depth: 10, colors: true }))

const func: Closure = expectMap(rootScope, "main", "No main function found");
const compiledFunction = functionTemplateTypeCheckAndCompile(func.func, [], [], rootScope);
// console.log(Bun.inspect(compiledFunction.body, { depth: 10, colors: true }));

compilerState.global.compiledFunctions.forEach((func) => {
  console.log(func.functionDefinition.debugName)
  console.log(Bun.inspect(func.body, { depth: 10, colors: true }));
  console.log("")
})