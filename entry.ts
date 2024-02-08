import { existsSync, mkdirSync, readFileSync, readdirSync, unlinkSync } from "fs";
import { CompilerError, ModuleLoader, compilerAssert, createDefaultGlobalCompiler, createScope, BuiltinTypes, Scope, GlobalExternalCompilerOptions, outputSourceLocation, SourceLocation, GlobalCompilerState, SubCompilerState, TaskContext, TokenRoot } from "./src/defs";
import { makeParser } from "./src/parser";
import { VecTypeMetaClass, preloadModuleText, print } from "./src/compiler_sugar";
import { extname, basename, normalize, dirname } from "path";
import { Queue, TaskDef, stepQueue, withContext } from "./src/tasks";
import { generateCompileCommands, programEntryTask } from "./src/compiler";
import { writeLlvmBytecode } from "./src/codegen_llvm";
import { exec } from "child_process";
import { FileSink } from "bun";

const globalOptions: GlobalExternalCompilerOptions = {
  libraryDirs: [`${import.meta.dir}/libs/`, `/opt/homebrew/lib/`],
  outputDir: `${import.meta.dir}/radius-build/`,
  llcPath: `/opt/homebrew/opt/llvm/bin/llc`,
  clangPath: `/usr/bin/clang`,
  importPaths: [
    `${import.meta.dir}/libs/`,
  ]
}

class BuildObject {
  constructor(
    public moduleName: string,
    public inputPath: string,
    public globalOptions: GlobalExternalCompilerOptions,
    public globalCompiler: GlobalCompilerState,
    public input: string,
    public debugOutputPath: string,
    public gotError = false
  ) {}
}

const logger = {
  logs: [],
  print: false,
  debugWriter: null as FileSink | null,
  log: (...args) => {
    if (logger.print) console.log(...args)
    if (logger.debugWriter) writeToFile(logger.debugWriter, ...args)
    logger.logs.push(args)
  },
}

const writeToFile = (writer: FileSink, ...args: unknown[]) => {
  args.forEach((arg) => {
    if (typeof arg === 'string') {
      writer.write(arg)
    } else writer.write(Bun.inspect(arg, { depth: 10, colors: true }))
    writer.write(' ')
  })
  writer.write('\n')
}

const loadBuildObject = (inputPath: string, globalOptions: GlobalExternalCompilerOptions) => {

  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.logger = logger
  globalCompiler.moduleLoader = createModuleLoader(globalOptions.importPaths)

  const moduleName = basename(inputPath, '.rad')

  const debugOutputPath = `${globalOptions.outputDir}${moduleName}.txt`
  const llPath = `${globalOptions.outputDir}${moduleName}.ll`
  const assemblyPath = `${globalOptions.outputDir}${moduleName}.s`
  const nativePath = `${globalOptions.outputDir}${moduleName}.native`

  globalCompiler.externalCompilerOptions.buildName = moduleName
  globalCompiler.externalCompilerOptions.llPath = llPath
  globalCompiler.externalCompilerOptions.assemblyPath = assemblyPath
  globalCompiler.externalCompilerOptions.nativePath = nativePath
  globalCompiler.externalCompilerOptions.globalOptions = globalOptions

  const input = readFileSync(inputPath, 'utf-8')

  const rootScope: Scope = createScope(
    {
      ...BuiltinTypes,
      print: print,
      // static_print: new ExternalFunction('static_print', new Binding('static_print', FunctionType), VoidType, (...args) => {
      //   logger.log('static_print called', ...args)
      //   prints.push(...args)
      //   return args[0]
      // }),

      VecType: VecTypeMetaClass,
    },
    undefined
  )
  globalCompiler.rootScope = rootScope

  if (existsSync(debugOutputPath)) unlinkSync(debugOutputPath)
  const file = Bun.file(debugOutputPath)
  const writer = file.writer()
  logger.debugWriter = writer

  const build = new BuildObject(moduleName, inputPath, globalOptions, globalCompiler, input, debugOutputPath)

  return build

}

const runCompiler = async (inputPath: string) => {

  const build = loadBuildObject(inputPath, globalOptions)

  const queue = new Queue()

  try {
    runModuleInner(queue, build.input, `${build.moduleName}.rad`, build.globalCompiler, build.rootScope)
  } catch (ex) {
    logger.print = true
    handleError(build, ex)
  }

  writeLlvmBytecodeFile(build)
  await executeLlvmCompiler(build)

  if (logger.debugWriter) logger.debugWriter.end()
}

const handleError = (build: BuildObject, ex) => {
  build.gotError = true

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
  }

  if (logger.debugWriter) logger.debugWriter.end()
}

const runModuleInner = (
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

const createModuleLoader = (importPaths: string[]) => {

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

const writeLlvmBytecodeFile = (build: BuildObject) => {
  compilerAssert(build.globalCompiler, "Not compiled")
  const path = build.globalCompiler.externalCompilerOptions.llPath
  mkdirSync(dirname(path), { recursive: true });
  if (existsSync(path)) unlinkSync(path)
  const file = Bun.file(path)
  const bytecodeWriter = file.writer()
  try {
    writeLlvmBytecode(build.globalCompiler, bytecodeWriter)
  } catch(ex) {
    handleError(build, ex)
    throw ex
  } finally {
    bytecodeWriter.end()
  }
}

const execPromise = (command: string) => {
  return new Promise((resolve, reject) => {
    exec(command, (err, out) => { 
      if (err) reject(err); else resolve(out)
    })
  })
}

const executeLlvmCompiler = async (build: BuildObject) => {
  compilerAssert(build.globalCompiler, "Not compiled")
  const cmds = generateCompileCommands(build.globalCompiler!)
  await execPromise(cmds.compile)
  await execPromise(cmds.link)
  console.log(`Built native executable\n${build.globalCompiler.externalCompilerOptions.nativePath}`)
}

const args = [...process.argv]
args.shift()
args.shift()
compilerAssert(args.length === 1)
runCompiler(args[0])