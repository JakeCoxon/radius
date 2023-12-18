import { functionTemplateTypeCheckAndCompileTask, runTopLevel } from "./compiler";
import { BoolType, Closure, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, VoidType, compilerAssert, createScope, expectMap } from "./defs";
import { makeParser } from "./parser"
import { Queue, stepQueue, withContext } from "./tasks";

const parser = makeParser(`


foo2 :: foo + 3
foo :: thing4(5)


defn thing(x: int):
  print(x + 32)

defn thing4(x: int):
  return x + 2

defn fam!(T)(a: int):
  print(a)
  print(T)

defn famz!(T)(a: T):
  print(a)

defn foop():
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

  zzz := 3
  z : int = 2
  x : int = z ifx 2 else 2

  #while false or true:
  #  print("OK")
  
  if 3:
    print("asd")

  meta if thing4(2):
    print("k thing4")

  meta if true:
    print("this is meta if")

  foo1 :: {|x| x + 1}

  foo1(1)
  fam!2(1)
  fam!(1 + 1)(2)

  famz!int(200)
  famz!bool(true)
  
  print(meta thing(12))

defn main():
  print(2 + 42 * thing(12))

  meta foop()

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

  zzz := 3
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
  fam!(1 + 1)(2)

  famz!int(200)
  famz!bool(true)
  
  print(meta thing(12))

  #for x in thing:
  #  print(x)


`)


const rootScope: Scope = createScope({
  int: IntType,
  float: FloatType,
  double: DoubleType,
  void: VoidType,
  string: StringType,
  bool: BoolType,
  compfoo: { _function: (a, b) => 65 + a + b },
  bar: 123,
  print: new ExternalFunction('print', (...args) => {
    console.log("print called", ...args);
    return args[0];
  }),
});

// pushSubCompilerState({ })




// console.log(Bun.inspect(parser.ast, { depth: 10, colors: true }))
const globalCompiler: GlobalCompilerState = {
  compiledFunctions: new Map(),
  functionDefinitions: [],
  subCompilerState: undefined
}

const queue = new Queue();
// queue.enqueue(functionTemplateTypeCheckAndCompileDef.of({ func: func.func, args: [], typeArgs: [], parentScope: rootScope}))
const root = (
  runTopLevel(globalCompiler, parser.ast, rootScope)
  .chainFn((task, arg) => {
    const func: Closure = expectMap(rootScope, "main", "No main function found");
    return functionTemplateTypeCheckAndCompileTask.create({ func: func.func, args: [], typeArgs: [], parentScope: func.scope, })
  })
  .wrap(withContext({ globalCompiler }))
);
queue.enqueue(root)

for (let i = 0; i < 10000; i++) {
  if (queue.list.length === 0) break;
  stepQueue(queue);
}

// console.log(Bun.inspect(root, { depth: 3, colors: true }))
compilerAssert(root._success, "Expected success", { root })
// compilerAssert(queue.list.length === 0, "Expected empty queue")
// console.log(queue.list.length)


// const compiledFunction = functionTemplateTypeCheckAndCompile(func.func, [], [], rootScope);
// // console.log(Bun.inspect(compiledFunction.body, { depth: 10, colors: true }));

globalCompiler.compiledFunctions.forEach((func) => {
  console.log(func.functionDefinition.debugName)
  console.log(Bun.inspect(func.body, { depth: 10, colors: true }));
  console.log("")
})
