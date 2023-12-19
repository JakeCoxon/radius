import { functionTemplateTypeCheckAndCompileTask, runTopLevelTask } from "./compiler";
import { BoolType, Closure, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, VoidType, compilerAssert, createScope, expectMap } from "./defs";
import { makeParser } from "./parser"
import { Queue, TaskDef, stepQueue, withContext } from "./tasks";

const parser = makeParser(`


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
  classDefinitions: [],
  subCompilerState: undefined
}

const queue = new Queue();
// queue.enqueue(functionTemplateTypeCheckAndCompileDef.of({ func: func.func, args: [], typeArgs: [], parentScope: rootScope}))
const root = (
  TaskDef(runTopLevelTask, globalCompiler, parser.ast, rootScope)
  .chainFn((task, arg) => {
    const func: Closure = expectMap(rootScope, "main", "No main function found");
    return TaskDef(functionTemplateTypeCheckAndCompileTask, { func: func.func, args: [], typeArgs: [], parentScope: func.scope, concreteTypes: [] })
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
