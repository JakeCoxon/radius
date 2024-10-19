import { existsSync, unlinkSync, readFileSync, readdirSync } from 'node:fs'
import { generateCompileCommands, programEntryTask } from '../src/compiler'
import { BoolType, Closure, CompilerError, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, SubCompilerState, TaskContext, VoidType, compilerAssert, createDefaultGlobalCompiler, createScope, expectMap, BuiltinTypes, ModuleLoader, SourceLocation, textColors, outputSourceLocation, TokenRoot, ParseImport, createAnonymousToken, Logger, Binding, FunctionType, GlobalExternalCompilerOptions, BuildObject, Type, Capability, CompiledFunction, FunctionParameter, Ast, RawPointerType } from "../src/defs"; // prettier-ignore
import { makeParser } from '../src/parser'
import { Queue, TaskDef, stepQueue, withContext } from '../src//tasks'
import { expect } from 'bun:test'
import { VecTypeMetaClass, externalBuiltinBindings, generateTypeMethods, preloadModuleText, print } from '../src/compiler_sugar'
import { FileSink } from 'bun';
import { writeLlvmBytecode } from '../src/codegen_llvm';
import { basename, extname, normalize } from 'node:path';
import { exec } from 'node:child_process';
import { writeSyntax } from '../src/codegen_syntax';
import { CodeGenerator, FunctionCodeGenerator } from '../borrow/codegen_ir';
import { FunctionBlock, Module, printIR } from '../borrow/defs';
import { generateConstructor, generateMoveFunction } from '../borrow/codegen_ast';
import { writeLlvmBytecodeBorrow } from '../borrow/codegen_llvm';
import { buildCFG, printCFG, printDominators } from '../borrow/controlflow';
import { ReifyAccessPass } from '../borrow/reifyaccess';
import { InitializationCheckingPass } from '../borrow/initialization';
import { insertCloseAccesses } from '../borrow/liveness';
import { ExclusivityCheckingPass } from '../borrow/exclusivity';

const runTestInner = (
  testObject: TestObject,
  queue: Queue,
  input: string,
  filepath: string,
  globalCompiler: GlobalCompilerState,
) => {
  const parser = makeParser(input, filepath)

  const subCompilerState = new SubCompilerState('testmodule')
  const moduleScope = createScope({ ...globalCompiler.rootScope }, undefined)
  subCompilerState.scope = moduleScope
  subCompilerState.globalCompiler = globalCompiler
  subCompilerState.moduleCompiler = subCompilerState

  const root = TaskDef(programEntryTask, parser)
    .wrap(withContext({ globalCompiler, subCompilerState } as TaskContext))
  queue.enqueue(root)

  let i
  const STEPS = 1_000_000
  for (i = 0; i < STEPS; i++) {
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
  if (!root._success && i === STEPS) {
    compilerAssert(false, 'Exhausted. maybe an infinite loop', { root })
  }
  compilerAssert(root._success, 'Expected success', { root })
  testObject.logger.log('Completed in num steps:', i)
}

export const createModuleLoader = (importPaths: string[]) => {

  const filesByName: {[key:string]: string} = {}
  importPaths.forEach(importPath => {
    const files = readdirSync(importPath).filter(x => extname(x) === '.rad')
    files.forEach(file => {
      const name = basename(file, extname(file))
      if (filesByName[name]) return
      filesByName[name] = normalize(`${importPath}${file}`)
    })
  })
  
  return <ModuleLoader>{
    cache: {},
    loadModule: (module) => {
      if (module === '_preload') return makeParser(preloadModuleText(), '_preload')
      compilerAssert(filesByName[module], "No module found $module", { module, importPaths })
      const input = readFileSync(filesByName[module], 'utf-8')
      return makeParser(input, filesByName[module])
    },
  }
}

const originalLog = console.log

const runMandatoryPasses = (fnGenerator: FunctionCodeGenerator, mod: Module, fn: FunctionBlock, body: Ast) => {
  const DebugLog = true

  console.log(textColors.yellow(`\n// ${fn.name} ///////////////////////////////////////////////////////////\n`));
  fn.params.forEach((p, i) => {
    console.log(textColors.yellow(`// ${p.binding.name}: ${p.capability} ${p.type.shortName} - ref ${p.reference} - ${fn.parameterRegisters[i]}`));
  })

  // console.dir(body, { depth: 10 });


  // Filter out blocks that are not reachable from the entry block
  const cfgFirst = buildCFG(fn.blocks)
  fn.blocks = cfgFirst.blocks.filter(b => cfgFirst.predecessors.get(b)!.length > 0 || b === cfgFirst.entry)
  
  printIR(fn.blocks);

  const cfg = buildCFG(fn.blocks)
  printCFG(cfg)
  printDominators(cfg)

  const reify = new ReifyAccessPass(cfg);
  reify.debugLog = DebugLog;
  reify.reifyAccesses();

  console.log("Reified")
  printIR(fn.blocks);

  const interpreter = new InitializationCheckingPass(fnGenerator, mod, fn);
  interpreter.debugLog = DebugLog;
  interpreter.checkedInterpret();

  console.log("Initialized")
  printIR(fn.blocks);

  console.log("")
  insertCloseAccesses(cfg, fn.blocks, DebugLog)

  console.log("Closed access")
  printIR(fn.blocks);

  const interpreter2 = new ExclusivityCheckingPass(fn)
  interpreter2.debugLog = DebugLog;
  interpreter2.checkedInterpret();
  console.log("")

  console.log(``);
  printIR(fn.blocks);
  console.log(`\n/// finished ${fn.name} ///\n`);
}

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
      compfoo: { _function: (a: number, b: number) => 65 + a + b },
      print: print,
      static_print: new ExternalFunction('static_print', VoidType, (ctx, args: unknown[]) => {
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
  globalCompiler.initializerFunctionBinding = externalBuiltinBindings.initializer
  globalCompiler.logger = logger
  globalCompiler.moduleLoader = moduleLoader || createModuleLoader(testObject.globalOptions.importPaths)
  globalCompiler.rootScope = rootScope

  testObject.globalCompiler = globalCompiler

  let gotError = false
  let fatalError = false

  const writeBytecodeFile = () => {
    const path = testObject.rawPath
    if (existsSync(path)) unlinkSync(path)
    const file = Bun.file(path)
    const bytecodeWriter = file.writer()
    // writeFinalBytecode(globalCompiler, bytecodeWriter)
    bytecodeWriter.end()
  }

  globalCompiler.externalCompilerOptions.buildName = testObject.moduleName
  globalCompiler.externalCompilerOptions.llPath = testObject.llPath
  globalCompiler.externalCompilerOptions.assemblyPath = testObject.assemblyPath
  globalCompiler.externalCompilerOptions.nativePath = testObject.nativePath

  globalCompiler.externalCompilerOptions.globalOptions = testObject.globalOptions

  {
    const printInt = externalBuiltinBindings.printInt
    const printArg = new Binding('printArg', VoidType)
    globalCompiler.compiledFunctions.set(printInt,
      new CompiledFunction(printInt, { debugName: "printInt" } as any, VoidType, [IntType], null!, [printArg], [
        new FunctionParameter(printArg, IntType, false, IntType, Capability.Let)
      ], [], 0))
  }
  {
    const malloc = externalBuiltinBindings.malloc
    const sizeArg = new Binding('sizeArg', VoidType)
    globalCompiler.compiledFunctions.set(malloc,
      new CompiledFunction(malloc, { debugName: "malloc" } as any, RawPointerType, [IntType], null!, [sizeArg], [
        new FunctionParameter(sizeArg, IntType, false, IntType, Capability.Let)
      ], [], 0))
  }
  {
    const free = externalBuiltinBindings.free
    const pointerArg = new Binding('pointerArg', VoidType)
    globalCompiler.compiledFunctions.set(free,
      new CompiledFunction(free, { debugName: "free" } as any, RawPointerType, [IntType], null!, [pointerArg], [
        new FunctionParameter(pointerArg, RawPointerType, false, RawPointerType, Capability.Let)
      ], [], 0))
  }

  try {
    runTestInner(testObject, queue, input, `${testObject.moduleName}.rad`, globalCompiler)

    globalCompiler.compiledFunctions.forEach((func) => {
      writer.write(func.functionDefinition.debugName)
      writer.write('\n')
      writer.write(Bun.inspect(func.body, { depth: 100, colors: true }))
      writer.write('\n\n')
    })

    generateTypeMethods(globalCompiler, StringType)

    const codeGenerator = new CodeGenerator();
    const mod = new Module()
    mod.functionMap = globalCompiler.compiledFunctions
    codeGenerator.functions = globalCompiler.compiledFunctions

    globalCompiler.compiledIr = new Map()
    globalCompiler.compiledFunctions.forEach((func) => {
      if (!func.body) return
      const fnGenerator = codeGenerator.functionGenerator(func)
      const fn = fnGenerator.generateFunction(func.binding, func.parameters, func.returnType, func.body)
      runMandatoryPasses(fnGenerator, mod, fn, func.body)

      globalCompiler.compiledIr.set(func.binding, fn)
    })

    // writeLlvmBytecodeBorrow(globalCompiler, writer)

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
        ;(ex.info as any)._userinfo.forEach((name: string) => {
          const item = ex.info[name]
          if (item && Object.getPrototypeOf(item) === TokenRoot) {
            const text = outputSourceLocation(item.location)
            logger.log(text)
          }
        })
      }

      logger.log('\nError info')
      Object.entries(ex.info).forEach(([name, value]) => {
        let str = ''
        try {
          str = Bun.inspect(value, { depth: 10, colors: true })
        } catch(ex) {
          str = `<error printing: ${ex.message}>` // sometimes theres a JSON cyclic error
        }
        logger.log(`${name}:`, str)
      })
      if ((ex.info as any).fatal) fatalError = true
    }
  }

  testObject.fail = gotError

  expect(gotError).toBe(expectError)
  expect(fatalError).toBe(false)

  return { prints, globalCompiler }
}

export const logError = (ex: Error, logger: Logger) => {
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
      ;(ex.info as any)._userinfo.forEach((name: string) => {
        const item = (ex.info as any)[name]
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
  const path = testObject.llPath
  if (existsSync(path)) unlinkSync(path)
  const file = Bun.file(path)
  const bytecodeWriter = file.writer()
  try {
    writeLlvmBytecodeBorrow(testObject.globalCompiler, bytecodeWriter)
  } catch(ex) {
    // console.log(ex)
    logError(ex, testObject.logger)
    throw ex
  }
  bytecodeWriter.end()
  testObject.writer.write("LLVm file")
  testObject.writer.write(await file.text())
}

export const writeSyntaxFile = async (testObject: TestObject) => {
  compilerAssert(testObject.globalCompiler, "Not compiled")
  const path = testObject.syntaxPath
  if (existsSync(path)) unlinkSync(path)
  const file = Bun.file(path)
  const writer = file.writer()
  try {
    writeSyntax(testObject.globalCompiler, writer)
  } catch(ex) {
    // console.log(ex)
    logError(ex, testObject.logger)
    throw ex
  }
  writer.end()
  testObject.writer.write("LLVm file")
  testObject.writer.write(await file.text())
}


const execPromise = (command: string) => {
  console.log(command)
  return new Promise<string>((resolve, reject) => {
    exec(command, (err, out) => { 
      if (err) {
        console.log(out)
        reject(err)
      } else resolve(out)
    })
  })
}


const executeLlvmCompiler = async (build: BuildObject) => {
  compilerAssert(build.globalCompiler, "Not compiled")
  const cmds = generateCompileCommands(build.globalCompiler!)
  // await execPromise(cmds.compile)
  await execPromise(cmds.compileAndLink)
  console.log(`Built native executable\n${build.globalCompiler.externalCompilerOptions.nativePath}`)
}

export const executeNativeExecutable = async (testObject: TestObject) => {
  if (testObject.fail) return
  const build = new BuildObject(testObject.moduleName, testObject.inputPath, testObject.globalOptions, testObject.globalCompiler!, '', '')
  await executeLlvmCompiler(build)
  await execPromise(testObject.nativePath).then(out => {
    console.log(out)
  })
}

type TestObject = { 
  globalOptions: GlobalExternalCompilerOptions,
  moduleName: string
  outputPath: string
  inputPath: string
  rawPath: string
  assemblyPath: string,
  nativePath: string,
  llPath: string
  syntaxPath: string
  logger: Logger
  writer: FileSink,
  globalCompiler?: GlobalCompilerState,
  fail: boolean
  prints: unknown[]
  close: () => void
}
export const createTest = ({ 
    moduleName, inputPath, globalOptions } : { 
        globalOptions: GlobalExternalCompilerOptions,
        moduleName: string,
        inputPath: string,
      }) => {
  const outputPath = `${globalOptions.outputDir}${moduleName}.txt`
  const rawPath = `${globalOptions.outputDir}${moduleName}.raw`
  const llPath = `${globalOptions.outputDir}${moduleName}.ll`
  const assemblyPath = `${globalOptions.outputDir}${moduleName}.s`
  const nativePath = `${globalOptions.outputDir}${moduleName}.native`
  const syntaxPath = `${globalOptions.outputDir}${moduleName}.compiled.rad`

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
  globalThis.console.dir = (arg, opts) => {
    originalLog(Bun.inspect(arg, { depth: opts.depth, colors: true }))
    logger.log(Bun.inspect(arg, { depth: opts.depth, colors: true }))
  }
  ;(globalThis as any).logger = logger

  const close = () => {
    writer.flush()
    writer.end()
  }

  return <TestObject>{ 
    moduleName, fail: false, nativePath, 
    assemblyPath, inputPath, globalOptions, 
    rawPath, outputPath, llPath, syntaxPath,
    logger, writer, 
    close, prints: [] }
}

export const printCompileCommands = (testObject: TestObject) => {
  const cmds = generateCompileCommands(testObject.globalCompiler!)
  console.log(`${cmds.compileAndLink} && ${cmds.nativePath}`)
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