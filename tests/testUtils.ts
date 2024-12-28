import { existsSync, readFileSync, readdirSync, unlinkSync } from 'node:fs';
import { basename, extname, normalize } from 'node:path';
import { runCompiler } from '../src/compiler_interface';
import { VecTypeMetaClass, externalBuiltinBindings, print } from '../src/compiler_sugar';
import { BuildObject, BuiltinTypes, ExternalFunction, GlobalExternalCompilerOptions, ModuleLoader, Scope, VoidType, compilerAssert, createDefaultGlobalCompiler, createScope } from "../src/defs"; // prettier-ignore
import { makeParser } from '../src/parser';


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

const originalLog = console.log


type TestObject = { 
  buildObject: BuildObject,
  run: () => Promise<void>
  close: () => void
}
export const createTest = ({ 
    moduleName, inputPath, globalOptions } : { 
        globalOptions: GlobalExternalCompilerOptions,
        moduleName: string,
        inputPath: string,
      }) => {

  const rootScope: Scope = createScope(
    {
      ...BuiltinTypes,
      compfoo: { _function: (a: number, b: number) => 65 + a + b },
      print: print,
      static_print: new ExternalFunction('static_print', VoidType, (ctx, args: unknown[]) => {
        logger.log('static_print called', ...args)
        // prints.push(...args)
        return args[0]
      }),

      VecType: VecTypeMetaClass,
    },
    undefined
  )

  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.initializerFunctionBinding = externalBuiltinBindings.initializer // TODO: Fix this
  
  globalCompiler.moduleLoader = createModuleLoader(globalOptions.importPaths)
  globalCompiler.rootScope = rootScope

  const outputPath = `${globalOptions.outputDir}${moduleName}.txt`

  Object.assign(globalCompiler.externalCompilerOptions, {
    buildName: moduleName,
    
    rawPath: `${globalOptions.outputDir}${moduleName}.raw`,
    llPath: `${globalOptions.outputDir}${moduleName}.ll`,
    assemblyPath: `${globalOptions.outputDir}${moduleName}.s`,
    nativePath: `${globalOptions.outputDir}${moduleName}.native`,
    syntaxPath: `${globalOptions.outputDir}${moduleName}.compiled.rad`,

    globalOptions,
  })

  if (existsSync(outputPath)) unlinkSync(outputPath)
  const file = Bun.file(outputPath)
  const writer = file.writer()

  const writeToFile = (...args: any[]) => {
    args.forEach((arg) => {
      if (typeof arg === 'string') {
        writer.write(arg)
      } else writer.write(Bun.inspect(arg, { depth: 10, colors: true }))
      writer.write(' ')
    })
    writer.write('\n')
  }

  const logger = {
    log: (...args: any[]) => {
      writeToFile(...args)
    },
  }
  globalCompiler.logger = logger

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

  const run = async () => {
    const input = await Bun.file(inputPath).text()
    buildObject.input = input
    await runCompiler(buildObject)

    if (buildObject.gotError) {
      close()
      throw buildObject.gotError
    }
  }


  const buildObject = new BuildObject(moduleName, inputPath, globalOptions, globalCompiler, '', '')
  buildObject.logger = logger
  buildObject.debugWriter = writer

  const testObject = <TestObject>{ 
    logger, writer, buildObject,
    run,
    close
  }

  return testObject
}
