import { existsSync, mkdirSync, readdirSync, readFileSync, unlinkSync } from "fs";
import { runCodegenPasses } from "../region/passes";
import { writeSyntax } from "./codegen_syntax";
import { createDefaultTypeFunctions } from "./compiler_types";
import { BuildObject, CompilerError, DiagnosticLocation, GlobalCompilerState, ModuleLoader, ParsedModule, SourceLocation, SubCompilerState, TaskContext, TokenRoot, compilerAssert, createScope, outputSourceLocation } from "./defs";
import { makeParser } from "./parser";
import { Queue, TaskDef, stepQueue, withContext } from "./tasks";
import { basename, dirname, extname, normalize } from "path";
import { writeLlvmBytecodeBorrowRegion } from "../region/codegen_llvm_region";
import { exec } from "child_process";
import { programEntryTask } from "./compiler";

export const generateCompileCommands = (globalCompiler: GlobalCompilerState) => {
  const opts = globalCompiler.externalCompilerOptions
  const globalOptions = opts.globalOptions
  const libs = opts.libraries.map(x => `-l${x}`).join(" ")
  const frameworks = opts.macosFrameworks.map(x => `-framework ${x}`).join(" ")
  const addLibraryDirs = globalOptions.libraryDirs.map(x => `-L${x}`).join(" ")
  const llcPath = globalOptions.llcPath
  const llPath = opts.llPath
  const nativePath = opts.nativePath
  const assemblyPath = opts.assemblyPath
  const clang = globalOptions.clangPath
  const optimizeLevel = 3
  return {
    compile: `${llcPath} ${llPath} -O${optimizeLevel} -o ${assemblyPath}`,
    compileAndLink: `${clang} ${llPath} -O${optimizeLevel} -o ${nativePath} ${addLibraryDirs} ${libs} ${frameworks}`,
    nativePath
  }
}

const runModuleInner = (
  queue: Queue,
  entryModule: ParsedModule,
  globalCompiler: GlobalCompilerState,
) => {
  

  const subCompilerState = new SubCompilerState('entrymodule')
  const moduleScope = createScope({ ...globalCompiler.rootScope }, undefined)
  subCompilerState.scope = moduleScope
  subCompilerState.globalCompiler = globalCompiler
  subCompilerState.moduleCompiler = subCompilerState

  const root = TaskDef(programEntryTask, entryModule)
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
}

export const writeLlvmBytecodeFile = (build: BuildObject) => {
  compilerAssert(build.globalCompiler, "Not compiled")
  const path = build.globalCompiler.externalCompilerOptions.llPath
  compilerAssert(path, "No ll path")
  mkdirSync(dirname(path), { recursive: true });
  if (existsSync(path)) unlinkSync(path)
  const file = Bun.file(path)
  const bytecodeWriter = file.writer()
  try {
    writeLlvmBytecodeBorrowRegion(build.globalCompiler, bytecodeWriter)
  } catch(ex) {
    handleError(build, ex)
    throw ex
  } finally {
    bytecodeWriter.end()
  }
}

const handleError = (build: BuildObject, ex: Error) => {
  build.gotError = true
  const logger = build.logger

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
      const text = outputSourceLocation(new DiagnosticLocation(location, "here"))
      logger.log(text)
    }

    const diagnosticLocations = (ex.info as any).diagnosticLocations as DiagnosticLocation[]
    if (diagnosticLocations) {
      diagnosticLocations.forEach((location) => {
        const text = outputSourceLocation(location)
        logger.log(text)
      })
    }

    if ((ex.info as any)._userinfo) {
      ;(ex.info as any)._userinfo.forEach((name: string) => {
        const item = (ex.info as any)[name]
        if (item && Object.getPrototypeOf(item) === TokenRoot) {
          const text = outputSourceLocation(new DiagnosticLocation(location, "here"))
          logger.log(text)
        }
      })
    }

    logger.log('\nError info')
    Object.entries(ex.info).forEach(([name, value]) => {
      logger.log(`${name}:`, Bun.inspect(value, { depth: 10, colors: true }))
    })
  }

}



const execPromise = (command: string) => {
  console.log('\n' + command)
  return new Promise<string>((resolve, reject) => {
    exec(command, (err, out, stderr) => { 
      if (err) {
        console.log("----- OUT ----")
        console.log(out)
        console.log("----- ERROR -----")
        console.log(stderr)
        console.log(err)
        reject(err)
      } else {
        console.log("----- OUT ----")
        console.log(out)
        resolve(out)
      }
    })
  })
}


const executeLlvmCompiler = async (build: BuildObject) => {
  if (build.gotError) return
  compilerAssert(build.globalCompiler, "Not compiled")
  const cmds = generateCompileCommands(build.globalCompiler!)
  await execPromise(cmds.compileAndLink)
  console.log(`\nBuilt native executable\n${build.globalCompiler.externalCompilerOptions.nativePath}`)
}

const executeNativeExecutable = async (build: BuildObject) => {
  if (build.gotError) return
  const cmds = generateCompileCommands(build.globalCompiler!)
  await execPromise(cmds.nativePath)
}


export const createModuleLoaderFromFileSystem = (importPaths: string[]) => {

  const filesByName: {[key:string]: string} = {}
  importPaths.forEach(importPath => {
    const files = readdirSync(importPath).filter(x => extname(x) === '.rad')
    files.forEach(file => {
      const name = basename(file, extname(file))
      if (filesByName[name]) return
      filesByName[name] = normalize(`${importPath}${file}`)
    })
  })

  const preload = readFileSync(`${importPaths[0]}/preload.rad`, 'utf-8')
  compilerAssert(preload, "No preload module found")
  
  return <ModuleLoader>{
    cache: {},
    loadModule: (module) => {
      if (module === '_preload') return makeParser(preload, '_preload')
      compilerAssert(filesByName[module], "No module found $module", { module, importPaths })
      const input = readFileSync(filesByName[module], 'utf-8')
      return makeParser(input, filesByName[module])
    },
  }
}


export const runCompiler = async (build: BuildObject) => {

  const queue = new Queue()

  try {
    createDefaultTypeFunctions(build.globalCompiler)
    compilerAssert(build.input, "No input in build object")

    const entryModule = makeParser(build.input, `${build.moduleName}.rad`)

    runModuleInner(queue, entryModule, build.globalCompiler)

    if (build.debugWriter) 
      writeSyntax(build.globalCompiler, build.debugWriter);

    runCodegenPasses(build.globalCompiler)

    writeLlvmBytecodeFile(build)
    await executeLlvmCompiler(build)

    if (build.runExecutable) await executeNativeExecutable(build)
  } catch (ex) {
    build.gotError = ex
    handleError(build, ex)
  }

}