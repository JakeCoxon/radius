import { existsSync, unlinkSync, readFileSync } from 'node:fs'
import { programEntryTask } from '../src/compiler'
import { BoolType, Closure, CompilerError, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, SubCompilerState, TaskContext, VoidType, compilerAssert, createDefaultGlobalCompiler, createScope, expectMap, BuiltinTypes, ModuleLoader, SourceLocation, textColors, outputSourceLocation, TokenRoot, ParseImport, createAnonymousToken, Logger, Binding, FunctionType } from "../src/defs"; // prettier-ignore
import { makeParser } from '../src/parser'
import { Queue, TaskDef, stepQueue, withContext } from '../src//tasks'
import { expect } from 'bun:test'
import { VecTypeMetaClass, externals, preloadModuleText, print } from '../src/compiler_sugar'
import { writeFinalBytecode } from '../src/codegen'
import { FileSink } from 'bun';
import { writeLlvmBytecode } from '../src/codegen_llvm';

const runTestInner = (
  queue: Queue,
  input: string,
  filepath: string,
  globalCompiler: GlobalCompilerState,
  rootScope: Scope
) => {
  const parser = makeParser(input, filepath)

  const subCompilerState = new SubCompilerState('testmodule')
  const moduleScope = createScope({ ...rootScope }, undefined)
  subCompilerState.scope = moduleScope
  subCompilerState.globalCompiler = globalCompiler
  subCompilerState.moduleCompiler = subCompilerState

  const root = TaskDef(programEntryTask, parser, rootScope)
    .wrap(withContext({ globalCompiler, subCompilerState } as TaskContext))
  queue.enqueue(root)

  let i
  for (i = 0; i < 10000; i++) {
    if (queue.list.length === 0) {
      if (root._state !== 'completed') {
        // TODO: remove events after they are completed
        globalCompiler.allWaitingEvents.forEach((e) => e.failure({}))
      }
      if (queue.list.length === 0) break
    }
    stepQueue(queue)
  }
  if (root._failure) throw root._failure
  if (!root._success && i === 10000) {
    compilerAssert(false, 'Exhausted. maybe an infinite loop', { root })
  }
  compilerAssert(root._success, 'Expected success', { root })
}

export const createModuleLoader = (basepath: string) => {
  return <ModuleLoader>{
    cache: {},
    loadModule: (module) => {
      if (module === '_preload') return makeParser(preloadModuleText(), '_preload')
      const path = `${basepath}${module}.rad`
      const input = readFileSync(path, 'utf-8')
      return makeParser(input, path)
    },
  }
}

const originalLog = console.log

export const runCompilerTest = (
  input: string,
  {
    moduleLoader,
    expectError = false,
    testObject
  }: { moduleLoader?: ModuleLoader; testObject: TestObject, expectError?: boolean }
) => {
  const logger = testObject.logger;
  const writer = testObject.writer;

  const prints = testObject.prints

  const rootScope: Scope = createScope(
    {
      ...BuiltinTypes,
      compfoo: { _function: (a, b) => 65 + a + b },
      print: print,
      static_print: new ExternalFunction('static_print', new Binding('static_print', FunctionType), VoidType, (...args) => {
        logger.log('static_print called', ...args)
        prints.push(...args)
        return args[0]
      }),

      VecType: VecTypeMetaClass,
    },
    undefined
  )

  const queue = new Queue()

  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.logger = logger
  globalCompiler.moduleLoader = moduleLoader || createModuleLoader(`${import.meta.dir}/fixtures/imports/`)

  testObject.globalCompiler = globalCompiler

  let gotError = false
  let fatalError = false

  const writeBytecodeFile = () => {
    const path = testObject.rawPath
    if (existsSync(path)) unlinkSync(path)
    const file = Bun.file(path)
    const bytecodeWriter = file.writer()
    writeFinalBytecode(globalCompiler, bytecodeWriter)
    bytecodeWriter.end()
  }


  try {
    runTestInner(queue, input, `${testObject.moduleName}.rad`, globalCompiler, rootScope)

    globalCompiler.compiledFunctions.forEach((func) => {
      writer.write(func.functionDefinition.debugName)
      writer.write('\n')
      writer.write(Bun.inspect(func.body, { depth: 100, colors: true }))
      writer.write('\n\n')
    })

    // writeBytecodeFile()
  } catch (ex) {
    gotError = true

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
        ;(ex.info as any)._userinfo.forEach((name) => {
          const item = ex.info[name]
          if (item && Object.getPrototypeOf(item) === TokenRoot) {
            const text = outputSourceLocation(item.location)
            logger.log(text)
          }
        })
      }

      logger.log('\nError info')
      Object.entries(ex.info).forEach(([name, value]) => {
        logger.log(`${name}:`, Bun.inspect(value, { depth: 10, colors: true }))
      })
      if ((ex.info as any).fatal) fatalError = true
    }
  }

  testObject.fail = gotError

  expect(gotError).toBe(expectError)
  expect(fatalError).toBe(false)

  return { prints, globalCompiler }
}

const logError = (ex: Error, logger: Logger) => {
  if (ex instanceof Error) {
    // if (ex.stack) logger.log(ex.stack)
    logger.log(ex.toString())
  }
  if (ex instanceof CompilerError) {
    const location = (ex.info as any).location as SourceLocation
    if (location) {
      const text = outputSourceLocation(location)
      logger.log(text)
    }

    if ((ex.info as any)._userinfo) {
      ;(ex.info as any)._userinfo.forEach((name) => {
        const item = ex.info[name]
        if (item && Object.getPrototypeOf(item) === TokenRoot) {
          const text = outputSourceLocation(item.location)
          logger.log(text)
        }
      })
    }

    logger.log('\nError info')
    const o = Object.fromEntries(Object.entries(ex.info))
    logger.log(o)
    
  }
}


export const writeLlvmBytecodeFile = async (testObject: TestObject) => {
  compilerAssert(testObject.globalCompiler, "Not compiled")
  const path = testObject.llvmPath
  if (existsSync(path)) unlinkSync(path)
  const file = Bun.file(path)
  const bytecodeWriter = file.writer()
  try {
    writeLlvmBytecode(testObject.globalCompiler, bytecodeWriter)
  } catch(ex) {
    // console.log(ex)
    logError(ex, testObject.logger)
    throw ex
  }
  bytecodeWriter.end()
  testObject.writer.write("LLVm file")
  testObject.writer.write(await file.text())
}

type TestObject = { 
  moduleName: string
  outputPath: string
  inputPath: string
  rawPath: string
  llvmPath: string
  logger: Logger
  writer: FileSink,
  globalCompiler?: GlobalCompilerState,
  fail: boolean
  prints: unknown[]
  close: () => void
}
export const createTest = ({ moduleName, inputPath, outputPath, rawPath, llvmPath } : { moduleName: string, inputPath: string, outputPath: string, rawPath: string, llvmPath: string }) => {
  if (existsSync(outputPath)) unlinkSync(outputPath)
  const file = Bun.file(outputPath)
  const writer = file.writer()

  const writeToFile = (...args) => {
    args.forEach((arg) => {
      if (typeof arg === 'string') {
        writer.write(arg)
      } else writer.write(Bun.inspect(arg, { depth: 10, colors: true }))
      writer.write(' ')
    })
    writer.write('\n')
  }

  const logger = {
    log: (...args) => {
      writeToFile(...args)
    },
  }

  globalThis.console.log = (...args) => {
    originalLog(...args)
    logger.log(...args)
  }
  ;(globalThis as any).logger = logger

  const close = () => {
    writer.flush()
    writer.end()
  }

  return <TestObject>{ moduleName, fail: false, inputPath, rawPath, outputPath, llvmPath, logger, writer, close, prints: [] }
}

export const runVm = async ({ testObject}: { testObject: TestObject }) => {
  if (testObject.fail) return;

  const dir = "/Users/jake/Dev/vm/build"
  const vm = `${dir}/vm`
  
  const proc = Bun.spawn([vm, testObject.rawPath], {
    cwd: dir,
    env: { NO_DEBUG: "1" },
    stderr: 'pipe',
  });
  const text = await new Response(proc.stdout).text();
  const error = await new Response(proc.stderr).text();
  testObject.writer.write(`\n---- VM output ----\n\n`)
  testObject.writer.write(text)
  if (error) {
    testObject.writer.write(`\n---- VM error ----\n\n`)
    testObject.writer.write(error)
    testObject.fail = true
    expect(error).toBe('')
  }
  // console.log(text)
}