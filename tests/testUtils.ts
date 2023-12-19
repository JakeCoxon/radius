import { functionTemplateTypeCheckAndCompileTask, runTopLevelTask } from "../src/compiler";
import { BoolType, Closure, CompilerError, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, VoidType, compilerAssert, createScope, expectMap } from "../src/defs";
import { makeParser } from "../src/parser"
import { Queue, TaskDef, stepQueue, withContext } from "../src//tasks";

export const runCompilerTest = (input: string, { filename }: { filename: string }) => {

  const file = Bun.file(`${__dirname}/output/${filename}.txt`);
  const writer = file.writer();

  const parser = makeParser(input)
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

  try {
    for (let i = 0; i < 10000; i++) {
      if (queue.list.length === 0) break;
      stepQueue(queue);
    }

    compilerAssert(root._success, "Expected success", { root })

  } catch (ex) {
    if (ex instanceof Error) {
      if (ex.stack) logger.log(ex.stack)
      else logger.log(ex.toString())
    }
    if (ex instanceof CompilerError) {

      logger.log("\nError info")
      Object.entries(ex.info).forEach(([name, value]) => {
        logger.log(`${name}:`, Bun.inspect(value, { depth: 4, colors: true }))
      })
      
    }
  }

  globalCompiler.compiledFunctions.forEach((func) => {
    writer.write(func.functionDefinition.debugName)
    writer.write("\n")
    writer.write(Bun.inspect(func.body, { depth: 10, colors: true }));
    writer.write("\n\n")
    writer.flush(); // write buffer to disk
  })

  writer.end();

  return { prints, globalCompiler };

}