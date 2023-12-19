import { functionTemplateTypeCheckAndCompileTask, runTopLevelTask } from "../src/compiler";
import { BoolType, Closure, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, VoidType, compilerAssert, createScope, expectMap } from "../src/defs";
import { makeParser } from "../src/parser"
import { Queue, TaskDef, stepQueue, withContext } from "../src//tasks";

export const runCompilerTest = (input: string) => {

  const logger = process.env['LOGGER'] ? { log: console.log } : { log: (...args) => {} }

  const parser = makeParser(input)
  const prints: unknown[] = []

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
      logger.log("print called", ...args);
      prints.push(...args)
      return args[0];
    }),
  });


  // console.log(Bun.inspect(parser.ast, { depth: 10, colors: true }))
  const globalCompiler: GlobalCompilerState = {
    compiledFunctions: new Map(),
    functionDefinitions: [],
    classDefinitions: [],
    subCompilerState: undefined,
    logger
  }

  const queue = new Queue();

  const root = (
    TaskDef(runTopLevelTask, globalCompiler, parser.node, rootScope)
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

  return { prints, globalCompiler };

}