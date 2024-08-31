import { test, expect, describe } from 'bun:test'
import { createModuleLoader, createTest, executeNativeExecutable, logError, printCompileCommands, runCompilerTest, runVm, writeLlvmBytecodeFile, writeSyntaxFile } from './testUtils'
import { GlobalExternalCompilerOptions } from '../src/defs'
import { makeParser } from '../src/parser'

const globalOptions: GlobalExternalCompilerOptions = {
  libraryDirs: [`${import.meta.dir}/../libs/`, `/opt/homebrew/lib/`],
  outputDir: `${import.meta.dir}/output/`,
  llcPath: `/opt/homebrew/opt/llvm/bin/llc`,
  clangPath: `/usr/bin/clang`,
  importPaths: [
    `${import.meta.dir}/../libs/`,
    `${import.meta.dir}/fixtures/imports/`
  ]
}

test('superbasic', async () => {
  const testObject = createTest({ 
    moduleName: 'superbasic',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/superbasic.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('option', async () => {
  const testObject = createTest({ 
    moduleName: 'option',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/option.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeSyntaxFile(testObject)
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('basic', async () => {
  const testObject = createTest({ 
    moduleName: 'basic',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/basic.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})


test('closure', async () => {
  const testObject = createTest({ 
    moduleName: 'closure',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('closure2', async () => {
  const testObject = createTest({ 
    moduleName: 'closure2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure2.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('closure_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_binding',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_binding.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('closure_closed_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_closed_binding',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_closed_binding.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('closure_closed_inline_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_closed_inline_binding',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_closed_inline_binding.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('closure_compose', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_compose',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_compose.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('closure_escape', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_escape',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_escape.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('inline', async () => {
  const testObject = createTest({ 
    moduleName: 'inline',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/inline.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('inline_return', async () => {
  const testObject = createTest({ 
    moduleName: 'inline_return',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/inline_return.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeSyntaxFile(testObject)
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('inline_shadow', async () => {
  const testObject = createTest({ 
    moduleName: 'inline_shadow',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/inline_shadow.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('comptime', async () => {
  const testObject = createTest({ 
    moduleName: 'comptime',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/comptime.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    expect(testObject.prints).toEqual([5, 'thing', 12, 44, 1850, 'thing', 12, 44, 44, 'thing', 12, 44])
  } finally {
    testObject.close()
  }
})

test.todo('expressions', async () => {
  const testObject = createTest({ 
    moduleName: 'expressions',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/expressions.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('identifier_error', async () => {
  const testObject = createTest({ 
    moduleName: 'identifier_error',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/identifier_error.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject, expectError: true })
  } finally {
    testObject.close()
  }
})

test('identifier_error2', async () => {
  const testObject = createTest({ 
    moduleName: 'identifier_error2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/identifier_error2.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject, expectError: true })
  } finally {
    testObject.close()
  }
})

test('list_iterator', async () => {
  const testObject = createTest({ 
    moduleName: 'list_iterator',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/list_iterator.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('custom_iterator', async () => {
  const testObject = createTest({ 
    moduleName: 'custom_iterator',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/custom_iterator.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('noclosure', async () => {
  const testObject = createTest({ 
    moduleName: 'noclosure',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/noclosure.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject, expectError: true })
  } finally {
    testObject.close()
  }
})

test.todo('noshadow', async () => {
  const testObject = createTest({ 
    moduleName: 'noshadow',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/noshadow.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    expect(testObject.prints).toEqual([100, 100])
  } finally {
    testObject.close()
  }
})

test('struct', async () => {
  const testObject = createTest({ 
    moduleName: 'struct',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/struct.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('random', async () => {
  const testObject = createTest({ 
    moduleName: 'random',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/random.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('module', async () => {
  const testObject = createTest({ 
    moduleName: 'module',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/module.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('methods', async () => {
  const testObject = createTest({ 
    moduleName: 'methods',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/methods.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('methods2', async () => {
  const testObject = createTest({ 
    moduleName: 'methods2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/methods2.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('nomethod', async () => {
  const testObject = createTest({ 
    moduleName: 'nomethod',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/nomethod.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject, expectError: true })
  } finally {
    testObject.close()
  }
})

test('template_basic', async () => {
  const testObject = createTest({ 
    moduleName: 'template_basic',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/template_basic.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('template_advanced', async () => {
  const testObject = createTest({ 
    moduleName: 'template_advanced',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/template_advanced.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('list_struct', async () => {
  const testObject = createTest({ 
    moduleName: 'list_struct',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/list_struct.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('generic', async () => {
  const testObject = createTest({ 
    moduleName: 'generic',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/generic.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('generic2', async () => {
  const testObject = createTest({ 
    moduleName: 'generic2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/generic2.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('tuple', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/tuple.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('tuple_extract', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple_extract',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/tuple_extract.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeSyntaxFile(testObject)
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('tuple_return_type', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple_return_type',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/tuple_return_type.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})
test('dict', async () => {
  const testObject = createTest({ 
    moduleName: 'dict',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/dict.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('dict2', async () => {
  const testObject = createTest({ 
    moduleName: 'dict2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/dict2.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('transduce', async () => {
  const testObject = createTest({ 
    moduleName: 'transduce',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/transduce.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('list', async () => {
  const testObject = createTest({ 
    moduleName: 'list',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/list.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('range2d', async () => {
  const testObject = createTest({ 
    moduleName: 'range2d',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/range2d.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('vec', async () => {
  const testObject = createTest({ 
    moduleName: 'vec',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/vec.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})
test('named_break', async () => {
  const testObject = createTest({ 
    moduleName: 'named_break',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/named_break.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('ifs', async () => {
  const testObject = createTest({ 
    moduleName: 'ifs',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/ifs.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})
test('expansion', async () => {
  const testObject = createTest({ 
    moduleName: 'expansion',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/expansion.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('meta', async () => {
  const testObject = createTest({ 
    moduleName: 'meta',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/meta.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('advanced', async () => {
  const testObject = createTest({ 
    moduleName: 'advanced',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/advanced.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('reftype', async () => {
  const testObject = createTest({ 
    moduleName: 'reftype',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/reftype.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})
test('valtype', async () => {
  const testObject = createTest({ 
    moduleName: 'valtype',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/valtype.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('returns', async () => {
  const testObject = createTest({ 
    moduleName: 'returns',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/returns.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('global', async () => {
  const testObject = createTest({ 
    moduleName: 'global',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/global.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('compiler_module', async () => {
  const testObject = createTest({ 
    moduleName: 'compiler_module',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/compiler_module.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('numbers', async () => {
  const testObject = createTest({ 
    moduleName: 'numbers',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/numbers.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('refparam', async () => {
  const testObject = createTest({ 
    moduleName: 'refparam',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/refparam.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('refparam_method', async () => {
  const testObject = createTest({ 
    moduleName: 'refparam_method',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/refparam_method.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('array', async () => {
  const testObject = createTest({ 
    moduleName: 'array',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/array.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('ops', async () => {
  const testObject = createTest({ 
    moduleName: 'ops',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/ops.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('parser', async () => {
  const testObject = createTest({ 
    moduleName: 'parser',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/parser.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    const parser = makeParser(input, "parser")
  } catch(ex) {
    logError(ex, testObject.logger)
    throw ex
  } finally {
    testObject.close()
  }
})

test('binding_bug', async () => {
  const testObject = createTest({ 
    moduleName: 'binding_bug',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/binding_bug.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
  } finally {
    testObject.close()
  }
})

test('current_loop', async () => {
  const testObject = createTest({ 
    moduleName: 'current_loop',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/current_loop.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('add_operator_bug', async () => {
  const testObject = createTest({ 
    moduleName: 'add_operator_bug',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/add_operator_bug.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('overload', async () => {
  const testObject = createTest({ 
    moduleName: 'overload',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/overload.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('named_args', async () => {
  const testObject = createTest({ 
    moduleName: 'named_args',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/named_args.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('overload_error', async () => {
  const testObject = createTest({ 
    moduleName: 'overload_error',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/overload_error.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('generator', async () => {
  const testObject = createTest({ 
    moduleName: 'generator',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/generator.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeSyntaxFile(testObject)
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})

test('iterator_expansion', async () => {
  const testObject = createTest({ 
    moduleName: 'iterator_expansion',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/iterator_expansion.rad`,
  })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await writeSyntaxFile(testObject)
    await writeLlvmBytecodeFile(testObject)
    await executeNativeExecutable(testObject)
  } finally {
    testObject.close()
  }
})
