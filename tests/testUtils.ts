import { existsSync, unlinkSync, readFileSync } from "node:fs";
import { VecTypeMetaClass, runTopLevelTask } from "../src/compiler";
import { BoolType, Closure, CompilerError, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, SubCompilerState, TaskContext, VoidType, compilerAssert, createDefaultGlobalCompiler, createScope, expectMap, BuiltinTypes, ModuleLoader, SourceLocation, textColors, outputSourceLocation, TokenRoot } from "../src/defs";
import { makeParser } from "../src/parser"
import { Queue, TaskDef, stepQueue, withContext } from "../src//tasks";
import { expect } from "bun:test";
import { createCallAstFromValue, functionTemplateTypeCheckAndCompileTask } from "../src/compiler_functions";

const runTestInner = (queue: Queue, input: string, filepath: string, globalCompiler: GlobalCompilerState, rootScope: Scope) => {
  const parser = makeParser(input, filepath)
  
  const subCompilerState = new SubCompilerState('testmodule');
  const moduleScope = createScope({ ...rootScope }, undefined)
  subCompilerState.scope = moduleScope

  const root = (
    TaskDef(runTopLevelTask, parser.rootNode, rootScope, moduleScope)
    .chainFn((task, arg) => {
      const func: Closure = expectMap(moduleScope, "main", "No main function found");
      return TaskDef(createCallAstFromValue, func, [], [])
    })
    .wrap(withContext({ globalCompiler, subCompilerState } as TaskContext))
  );
  queue.enqueue(root)

  let i;
  for (i = 0; i < 10000; i++) {
    if (queue.list.length === 0) {
      if (root._state !== 'completed') {
        // TODO: remove events after they are completed
        globalCompiler.allWaitingEvents.forEach(e => e.failure({}))
      }
      if (queue.list.length === 0) break
    }
    stepQueue(queue);
  }
  if (root._failure) throw root._failure
  if (!root._success && i === 10000) {
    compilerAssert(false, "Exhausted. maybe an infinite loop", { root })
  }
  compilerAssert(root._success, "Expected success", { root })
}

export const createModuleLoader = (basepath: string) => {
  return <ModuleLoader>{
    cache: {},
    loadModule: (module) => {
      const path = `${basepath}${module}.rad`
      const input = readFileSync(path, 'utf-8')
      return makeParser(input, path)
    }
  }
}

const originalLog = console.log;

export const runCompilerTest = (input: string, { moduleLoader, filename, expectError=false }: { moduleLoader?: ModuleLoader, filename: string, expectError?: boolean }) => {

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

  globalThis.console.log = (...args) => {
    originalLog(...args)
    logger.log(...args)
  }

  const rootScope: Scope = createScope({
    ...BuiltinTypes,
    compfoo: { _function: (a, b) => 65 + a + b },
    print: new ExternalFunction('print', VoidType, (...args) => {
      logger.log("print called", ...args);
      prints.push(...args)
      return args[0];
    }),
    static_print: new ExternalFunction('static_print', VoidType, (...args) => {
      logger.log("static_print called", ...args);
      prints.push(...args)
      return args[0];
    }),

    VecType: VecTypeMetaClass
  }, undefined);

  const queue = new Queue();

  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.logger = logger
  globalCompiler.moduleLoader = moduleLoader || {
    cache: {},
    loadModule: (module) => {
      compilerAssert(false, "Not implemented")
    }
  }

  let gotError = false;
  let fatalError = false;
  

  try {
    runTestInner(queue, input, `${filename}.rad`, globalCompiler, rootScope)

  } catch (ex) {
    gotError = true;

    if (ex instanceof Error) {
      if (ex.stack) logger.log(ex.stack)
      else logger.log(ex.toString())
    }
    if (ex instanceof CompilerError) {

      // (ex.info as any).currentTask = (queue.currentTask as any)?.def;
      // (ex.info as any).subCompilerState = (queue.currentTask?._context as TaskContext).subCompilerState

      // logger.log("\nCompiler stack")
      const location = (ex.info as any).location as SourceLocation
      if (location) {
        const text = outputSourceLocation(location)
        logger.log(text)
      }

      if ((ex.info as any)._userinfo) {
        (ex.info as any)._userinfo.forEach(name => {
          const item = ex.info[name]
          if (item && Object.getPrototypeOf(item) === TokenRoot) {
            const text = outputSourceLocation(item.location)
            logger.log(text)
          }
        })
      }

      logger.log("\nError info")
      Object.entries(ex.info).forEach(([name, value]) => {
        logger.log(`${name}:`, Bun.inspect(value, { depth: 10, colors: true }))
      })
      if ((ex.info as any).fatal) fatalError = true
      
    }
  }

  globalCompiler.compiledFunctions.forEach((func) => {
    writer.write(func.functionDefinition.debugName)
    writer.write("\n")
    writer.write(Bun.inspect(func.body, { depth: 100, colors: true }));
    writer.write("\n\n")
  })

  writer.flush();
  writer.end();

  expect(gotError).toBe(expectError)
  expect(fatalError).toBe(false);

  return { prints, globalCompiler };

}