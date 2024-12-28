import { FileSink } from "bun";
import { existsSync, readFileSync, readdirSync, unlinkSync } from "fs";
import { basename, extname, normalize } from "path";
import { createModuleLoaderFromFileSystem, runCompiler } from "./src/compiler_interface";
import { VecTypeMetaClass, externalBuiltinBindings, print } from "./src/compiler_sugar";
import { BuildObject, BuiltinTypes, GlobalExternalCompilerOptions, ModuleLoader, Scope, compilerAssert, createDefaultGlobalCompiler, createScope } from "./src/defs";

// TODO: Fix these
const globalOptions: GlobalExternalCompilerOptions = {
  libraryDirs: [`${import.meta.dir}/libs/`, `/opt/homebrew/lib/`],
  outputDir: `${import.meta.dir}/radius-build/`,
  llcPath: `/opt/homebrew/opt/llvm/bin/llc`,
  clangPath: `/usr/bin/clang`,
  importPaths: [
    `${import.meta.dir}/libs/`,
  ]
}

const logger = {
  logs: [] as unknown[],
  print: false,
  debugWriter: null as FileSink | null,
  log: (...args: unknown[]) => {
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

const RootScope = {
  ...BuiltinTypes,
  print: print,
  // static_print: new ExternalFunction('static_print', new Binding('static_print', FunctionType), VoidType, (...args) => {
  //   logger.log('static_print called', ...args)
  //   prints.push(...args)
  //   return args[0]
  // }),

  VecType: VecTypeMetaClass,
}

const loadBuildObject = (inputPath: string, globalOptions: GlobalExternalCompilerOptions) => {

  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.logger = logger
  globalCompiler.moduleLoader = createModuleLoaderFromFileSystem(globalOptions.importPaths)
  globalCompiler.initializerFunctionBinding = externalBuiltinBindings.initializer // TODO: Fix this

  const moduleName = basename(inputPath, '.rad')

  const debugOutputPath = `${globalOptions.outputDir}${moduleName}.txt`
  const llPath = `${globalOptions.outputDir}${moduleName}.ll`
  const assemblyPath = `${globalOptions.outputDir}${moduleName}.s`
  const nativePath = `${globalOptions.outputDir}${moduleName}.native`

  Object.assign(globalCompiler.externalCompilerOptions, {
    buildName: moduleName,
    llPath,
    assemblyPath,
    nativePath,
    globalOptions,
  })

  const input = readFileSync(inputPath, 'utf-8')

  const rootScope: Scope = createScope(RootScope, undefined)
  globalCompiler.rootScope = rootScope

  if (existsSync(debugOutputPath)) unlinkSync(debugOutputPath)
  const file = Bun.file(debugOutputPath)
  const writer = file.writer()
  logger.debugWriter = writer
  // console.log = (...args) => writeToFile(logger.debugWriter!, ...args)

  const build = new BuildObject(moduleName, inputPath, globalOptions, globalCompiler, input, debugOutputPath)
  build.logger = logger
  logger.print = true
  build.debugWriter = writer

  return build

}


const main = async () => {
  const args = [...process.argv]
  args.shift()
  args.shift()
  if (args.length !== 1) {
    console.log("Expected single argument input file")
    process.exit(1)
  }
  const build = loadBuildObject(args[0], globalOptions)
  await runCompiler(build)

  if (build.gotError) process.exit(1)
}
main()