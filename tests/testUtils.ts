import { existsSync, unlinkSync } from "node:fs";
import { runTopLevelTask } from "../src/compiler";
import { BoolType, Closure, CompilerError, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, SubCompilerState, TaskContext, VoidType, compilerAssert, createDefaultGlobalCompiler, createScope, expectMap, BuiltinTypes } from "../src/defs";
import { makeParser } from "../src/parser"
import { Queue, TaskDef, stepQueue, withContext } from "../src//tasks";
import { expect } from "bun:test";
import { functionTemplateTypeCheckAndCompileTask } from "../src/compiler_functions";

const runTestInner = (queue: Queue, input: string, globalCompiler: GlobalCompilerState, rootScope: Scope) => {
  const parser = makeParser(input)
  
  const subCompilerState = new SubCompilerState('root');
  subCompilerState.scope = rootScope
  const root = (
    TaskDef(runTopLevelTask, parser.node, rootScope)
    .chainFn((task, arg) => {
      const func: Closure = expectMap(rootScope, "main", "No main function found");
      return TaskDef(functionTemplateTypeCheckAndCompileTask, { func: func.func, args: [], typeArgs: [], parentScope: func.scope, concreteTypes: [] })
    })
    .wrap(withContext({ globalCompiler, subCompilerState } as TaskContext))
  );
  queue.enqueue(root)

  for (let i = 0; i < 10000; i++) {
    if (queue.list.length === 0) break;
    stepQueue(queue);
  }

  if (root._state !== 'completed') {
    // TODO: remove events after they are completed
    globalCompiler.allWaitingEvents.forEach(e => e.failure({}))
  }
  compilerAssert(root._success, "Expected success", { root })
}

export const runCompilerTest = (input: string, { filename, expectError=false }: { filename: string, expectError?: boolean }) => {

  const path = `${import.meta.dir}/output/${filename}.txt`
  if (existsSync(path)) unlinkSync(path)
  const file = Bun.file(path);
  const writer = file.writer();
  
  const prints: unknown[] = []

  const writeToFile = (...args) => {
    args.forEach(arg => {
      if (typeof arg === 'string') {
        writer.write(arg)
      } else writer.write(Bun.inspect(arg, { depth: 10, colors: true }))
      writer.write(' ')
    })
    writer.write('\n')
  }

  const logger = { log: (...args) => {
    writeToFile(...args)
  } }

  const rootScope: Scope = createScope({
    ...BuiltinTypes,
    compfoo: { _function: (a, b) => 65 + a + b },
    bar: 123,
    print: new ExternalFunction('print', (...args) => {
      logger.log("print called", ...args);
      prints.push(...args)
      return args[0];
    }),
  });

  const queue = new Queue();

  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.logger = logger

  let gotError = false;
  let fatalError = false;
  

  try {
    runTestInner(queue, input, globalCompiler, rootScope)

  } catch (ex) {
    gotError = true;

    if (ex instanceof Error) {
      if (ex.stack) logger.log(ex.stack)
      else logger.log(ex.toString())
    }
    if (ex instanceof CompilerError) {

      (ex.info as any).currentTask = (queue.currentTask as any)?.def;
      (ex.info as any).subCompilerState = (queue.currentTask?._context as TaskContext).subCompilerState

      logger.log("\nError info")
      Object.entries(ex.info).forEach(([name, value]) => {
        logger.log(`${name}:`, Bun.inspect(value, { depth: 4, colors: true }))
      })
      if ((ex.info as any).fatal) fatalError = true
      
    }
  }

  globalCompiler.compiledFunctions.forEach((func) => {
    writer.write(func.functionDefinition.debugName)
    writer.write("\n")
    writer.write(Bun.inspect(func.body, { depth: 10, colors: true }));
    writer.write("\n\n")
  })

  writer.flush();
  writer.end();

  expect(gotError).toBe(expectError)
  expect(fatalError).toBe(false);

  return { prints, globalCompiler };

}