import { test, expect, describe } from 'bun:test'
import { createModuleLoader, createTest, runCompilerTest, runVm } from './testUtils'

test('basic', async () => {
  const testObject = createTest({ 
    moduleName: 'basic',
    inputPath: `${import.meta.dir}/fixtures/basic.rad`,
    outputPath: `${import.meta.dir}/output/basic.txt`,
    rawPath: `${import.meta.dir}/output/basic.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('closure', async () => {
  const testObject = createTest({ 
    moduleName: 'closure',
    inputPath: `${import.meta.dir}/fixtures/closure.rad`,
    outputPath: `${import.meta.dir}/output/closure.txt`,
    rawPath: `${import.meta.dir}/output/closure.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('closure2', async () => {
  const testObject = createTest({ 
    moduleName: 'closure2',
    inputPath: `${import.meta.dir}/fixtures/closure2.rad`,
    outputPath: `${import.meta.dir}/output/closure2.txt`,
    rawPath: `${import.meta.dir}/output/closure2.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('closure_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_binding',
    inputPath: `${import.meta.dir}/fixtures/closure_binding.rad`,
    outputPath: `${import.meta.dir}/output/closure_binding.txt`,
    rawPath: `${import.meta.dir}/output/closure_binding.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('closure_closed_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_closed_binding',
    inputPath: `${import.meta.dir}/fixtures/closure_closed_binding.rad`,
    outputPath: `${import.meta.dir}/output/closure_closed_binding.txt`,
    rawPath: `${import.meta.dir}/output/closure_closed_binding.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('closure_closed_inline_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_closed_inline_binding',
    inputPath: `${import.meta.dir}/fixtures/closure_closed_inline_binding.rad`,
    outputPath: `${import.meta.dir}/output/closure_closed_inline_binding.txt`,
    rawPath: `${import.meta.dir}/output/closure_closed_inline_binding.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('closure_compose', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_compose',
    inputPath: `${import.meta.dir}/fixtures/closure_compose.rad`,
    outputPath: `${import.meta.dir}/output/closure_compose.txt`,
    rawPath: `${import.meta.dir}/output/closure_compose.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('closure_escape', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_escape',
    inputPath: `${import.meta.dir}/fixtures/closure_escape.rad`,
    outputPath: `${import.meta.dir}/output/closure_escape.txt`,
    rawPath: `${import.meta.dir}/output/closure_escape.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('inline', async () => {
  const testObject = createTest({ 
    moduleName: 'inline',
    inputPath: `${import.meta.dir}/fixtures/inline.rad`,
    outputPath: `${import.meta.dir}/output/inline.txt`,
    rawPath: `${import.meta.dir}/output/inline.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('inline_shadow', async () => {
  const testObject = createTest({ 
    moduleName: 'inline_shadow',
    inputPath: `${import.meta.dir}/fixtures/inline_shadow.rad`,
    outputPath: `${import.meta.dir}/output/inline_shadow.txt`,
    rawPath: `${import.meta.dir}/output/inline_shadow.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('comptime', async () => {
  const testObject = createTest({ 
    moduleName: 'comptime',
    inputPath: `${import.meta.dir}/fixtures/comptime.rad`,
    outputPath: `${import.meta.dir}/output/comptime.txt`,
    rawPath: `${import.meta.dir}/output/comptime.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
    expect(testObject.prints).toEqual([5, '"thing"', 12, 44, 1850, '"thing"', 12, 44, 44, '"thing"', 12, 44])
  } finally {
    testObject.close()
  }
})

test.todo('expressions', async () => {
  const testObject = createTest({ 
    moduleName: 'expressions',
    inputPath: `${import.meta.dir}/fixtures/expressions.rad`,
    outputPath: `${import.meta.dir}/output/expressions.txt`,
    rawPath: `${import.meta.dir}/output/expressions.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('identifier_error', async () => {
  const testObject = createTest({ 
    moduleName: 'identifier_error',
    inputPath: `${import.meta.dir}/fixtures/identifier_error.rad`,
    outputPath: `${import.meta.dir}/output/identifier_error.txt`,
    rawPath: `${import.meta.dir}/output/identifier_error.raw` })
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
    inputPath: `${import.meta.dir}/fixtures/identifier_error2.rad`,
    outputPath: `${import.meta.dir}/output/identifier_error2.txt`,
    rawPath: `${import.meta.dir}/output/identifier_error2.raw` })
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
    inputPath: `${import.meta.dir}/fixtures/list_iterator.rad`,
    outputPath: `${import.meta.dir}/output/list_iterator.txt`,
    rawPath: `${import.meta.dir}/output/list_iterator.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('custom_iterator', async () => {
  const testObject = createTest({ 
    moduleName: 'custom_iterator',
    inputPath: `${import.meta.dir}/fixtures/custom_iterator.rad`,
    outputPath: `${import.meta.dir}/output/custom_iterator.txt`,
    rawPath: `${import.meta.dir}/output/custom_iterator.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('noclosure', async () => {
  const testObject = createTest({ 
    moduleName: 'noclosure',
    inputPath: `${import.meta.dir}/fixtures/noclosure.rad`,
    outputPath: `${import.meta.dir}/output/noclosure.txt`,
    rawPath: `${import.meta.dir}/output/noclosure.raw` })
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
    inputPath: `${import.meta.dir}/fixtures/noshadow.rad`,
    outputPath: `${import.meta.dir}/output/noshadow.txt`,
    rawPath: `${import.meta.dir}/output/noshadow.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
    expect(testObject.prints).toEqual([100, 100])
  } finally {
    testObject.close()
  }
})

test('struct', async () => {
  const testObject = createTest({ 
    moduleName: 'struct',
    inputPath: `${import.meta.dir}/fixtures/struct.rad`,
    outputPath: `${import.meta.dir}/output/struct.txt`,
    rawPath: `${import.meta.dir}/output/struct.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('random', async () => {
  const testObject = createTest({ 
    moduleName: 'random',
    inputPath: `${import.meta.dir}/fixtures/random.rad`,
    outputPath: `${import.meta.dir}/output/random.txt`,
    rawPath: `${import.meta.dir}/output/random.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('module', async () => {
  const testObject = createTest({ 
    moduleName: 'module',
    inputPath: `${import.meta.dir}/fixtures/module.rad`,
    outputPath: `${import.meta.dir}/output/module.txt`,
    rawPath: `${import.meta.dir}/output/module.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('methods', async () => {
  const testObject = createTest({ 
    moduleName: 'methods',
    inputPath: `${import.meta.dir}/fixtures/methods.rad`,
    outputPath: `${import.meta.dir}/output/methods.txt`,
    rawPath: `${import.meta.dir}/output/methods.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('methods2', async () => {
  const testObject = createTest({ 
    moduleName: 'methods2',
    inputPath: `${import.meta.dir}/fixtures/methods2.rad`,
    outputPath: `${import.meta.dir}/output/methods2.txt`,
    rawPath: `${import.meta.dir}/output/methods2.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('nomethod', async () => {
  const testObject = createTest({ 
    moduleName: 'nomethod',
    inputPath: `${import.meta.dir}/fixtures/nomethod.rad`,
    outputPath: `${import.meta.dir}/output/nomethod.txt`,
    rawPath: `${import.meta.dir}/output/nomethod.raw` })
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
    inputPath: `${import.meta.dir}/fixtures/template_basic.rad`,
    outputPath: `${import.meta.dir}/output/template_basic.txt`,
    rawPath: `${import.meta.dir}/output/template_basic.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('template_advanced', async () => {
  const testObject = createTest({ 
    moduleName: 'template_advanced',
    inputPath: `${import.meta.dir}/fixtures/template_advanced.rad`,
    outputPath: `${import.meta.dir}/output/template_advanced.txt`,
    rawPath: `${import.meta.dir}/output/template_advanced.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('list_struct', async () => {
  const testObject = createTest({ 
    moduleName: 'list_struct',
    inputPath: `${import.meta.dir}/fixtures/list_struct.rad`,
    outputPath: `${import.meta.dir}/output/list_struct.txt`,
    rawPath: `${import.meta.dir}/output/list_struct.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('generic', async () => {
  const testObject = createTest({ 
    moduleName: 'generic',
    inputPath: `${import.meta.dir}/fixtures/generic.rad`,
    outputPath: `${import.meta.dir}/output/generic.txt`,
    rawPath: `${import.meta.dir}/output/generic.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('generic2', async () => {
  const testObject = createTest({ 
    moduleName: 'generic2',
    inputPath: `${import.meta.dir}/fixtures/generic2.rad`,
    outputPath: `${import.meta.dir}/output/generic2.txt`,
    rawPath: `${import.meta.dir}/output/generic2.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('tuple', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple',
    inputPath: `${import.meta.dir}/fixtures/tuple.rad`,
    outputPath: `${import.meta.dir}/output/tuple.txt`,
    rawPath: `${import.meta.dir}/output/tuple.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('tuple_return_type', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple_return_type',
    inputPath: `${import.meta.dir}/fixtures/tuple_return_type.rad`,
    outputPath: `${import.meta.dir}/output/tuple_return_type.txt`,
    rawPath: `${import.meta.dir}/output/tuple_return_type.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})
test('dict', async () => {
  const testObject = createTest({ 
    moduleName: 'dict',
    inputPath: `${import.meta.dir}/fixtures/dict.rad`,
    outputPath: `${import.meta.dir}/output/dict.txt`,
    rawPath: `${import.meta.dir}/output/dict.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('dict2', async () => {
  const testObject = createTest({ 
    moduleName: 'dict2',
    inputPath: `${import.meta.dir}/fixtures/dict2.rad`,
    outputPath: `${import.meta.dir}/output/dict2.txt`,
    rawPath: `${import.meta.dir}/output/dict2.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('transduce', async () => {
  const testObject = createTest({ 
    moduleName: 'transduce',
    inputPath: `${import.meta.dir}/fixtures/transduce.rad`,
    outputPath: `${import.meta.dir}/output/transduce.txt`,
    rawPath: `${import.meta.dir}/output/transduce.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('list', async () => {
  const testObject = createTest({ 
    moduleName: 'list',
    inputPath: `${import.meta.dir}/fixtures/list.rad`,
    outputPath: `${import.meta.dir}/output/list.txt`,
    rawPath: `${import.meta.dir}/output/list.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('range2d', async () => {
  const testObject = createTest({ 
    moduleName: 'range2d',
    inputPath: `${import.meta.dir}/fixtures/range2d.rad`,
    outputPath: `${import.meta.dir}/output/range2d.txt`,
    rawPath: `${import.meta.dir}/output/range2d.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('vec', async () => {
  const testObject = createTest({ 
    moduleName: 'vec',
    inputPath: `${import.meta.dir}/fixtures/vec.rad`,
    outputPath: `${import.meta.dir}/output/vec.txt`,
    rawPath: `${import.meta.dir}/output/vec.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})
test('named_break', async () => {
  const testObject = createTest({ 
    moduleName: 'named_break',
    inputPath: `${import.meta.dir}/fixtures/named_break.rad`,
    outputPath: `${import.meta.dir}/output/named_break.txt`,
    rawPath: `${import.meta.dir}/output/named_break.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('ifs', async () => {
  const testObject = createTest({ 
    moduleName: 'ifs',
    inputPath: `${import.meta.dir}/fixtures/ifs.rad`,
    outputPath: `${import.meta.dir}/output/ifs.txt`,
    rawPath: `${import.meta.dir}/output/ifs.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})
test('expansion', async () => {
  const testObject = createTest({ 
    moduleName: 'expansion',
    inputPath: `${import.meta.dir}/fixtures/expansion.rad`,
    outputPath: `${import.meta.dir}/output/expansion.txt`,
    rawPath: `${import.meta.dir}/output/expansion.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('meta', async () => {
  const testObject = createTest({ 
    moduleName: 'meta',
    inputPath: `${import.meta.dir}/fixtures/meta.rad`,
    outputPath: `${import.meta.dir}/output/meta.txt`,
    rawPath: `${import.meta.dir}/output/meta.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('advanced', async () => {
  const testObject = createTest({ 
    moduleName: 'advanced',
    inputPath: `${import.meta.dir}/fixtures/advanced.rad`,
    outputPath: `${import.meta.dir}/output/advanced.txt`,
    rawPath: `${import.meta.dir}/output/advanced.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('reftype', async () => {
  const testObject = createTest({ 
    moduleName: 'reftype',
    inputPath: `${import.meta.dir}/fixtures/reftype.rad`,
    outputPath: `${import.meta.dir}/output/reftype.txt`,
    rawPath: `${import.meta.dir}/output/reftype.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})
test('valtype', async () => {
  const testObject = createTest({ 
    moduleName: 'valtype',
    inputPath: `${import.meta.dir}/fixtures/valtype.rad`,
    outputPath: `${import.meta.dir}/output/valtype.txt`,
    rawPath: `${import.meta.dir}/output/valtype.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('returns', async () => {
  const testObject = createTest({ 
    moduleName: 'returns',
    inputPath: `${import.meta.dir}/fixtures/returns.rad`,
    outputPath: `${import.meta.dir}/output/returns.txt`,
    rawPath: `${import.meta.dir}/output/returns.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('global', async () => {
  const testObject = createTest({ 
    moduleName: 'global',
    inputPath: `${import.meta.dir}/fixtures/global.rad`,
    outputPath: `${import.meta.dir}/output/global.txt`,
    rawPath: `${import.meta.dir}/output/global.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('compiler_module', async () => {
  const testObject = createTest({ 
    moduleName: 'compiler_module',
    inputPath: `${import.meta.dir}/fixtures/compiler_module.rad`,
    outputPath: `${import.meta.dir}/output/compiler_module.txt`,
    rawPath: `${import.meta.dir}/output/compiler_module.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('numbers', async () => {
  const testObject = createTest({ 
    moduleName: 'numbers',
    inputPath: `${import.meta.dir}/fixtures/numbers.rad`,
    outputPath: `${import.meta.dir}/output/numbers.txt`,
    rawPath: `${import.meta.dir}/output/numbers.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('refparam', async () => {
  const testObject = createTest({ 
    moduleName: 'refparam',
    inputPath: `${import.meta.dir}/fixtures/refparam.rad`,
    outputPath: `${import.meta.dir}/output/refparam.txt`,
    rawPath: `${import.meta.dir}/output/refparam.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('refparam_method', async () => {
  const testObject = createTest({ 
    moduleName: 'refparam_method',
    inputPath: `${import.meta.dir}/fixtures/refparam_method.rad`,
    outputPath: `${import.meta.dir}/output/refparam_method.txt`,
    rawPath: `${import.meta.dir}/output/refparam_method.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('array', async () => {
  const testObject = createTest({ 
    moduleName: 'array',
    inputPath: `${import.meta.dir}/fixtures/array.rad`,
    outputPath: `${import.meta.dir}/output/array.txt`,
    rawPath: `${import.meta.dir}/output/array.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('ops', async () => {
  const testObject = createTest({ 
    moduleName: 'ops',
    inputPath: `${import.meta.dir}/fixtures/ops.rad`,
    outputPath: `${import.meta.dir}/output/ops.txt`,
    rawPath: `${import.meta.dir}/output/ops.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    await runVm({ testObject })
  } finally {
    testObject.close()
  }
})

test('cells', async () => {
  const testObject = createTest({ 
    moduleName: 'cells',
    inputPath: `${import.meta.dir}/fixtures/cells.rad`,
    outputPath: `${import.meta.dir}/output/cells.txt`,
    rawPath: `${import.meta.dir}/output/cells.raw` })
  try {
    const input = await Bun.file(testObject.inputPath).text()
    runCompilerTest(input, { testObject })
    // Don't run this because it runs a window
  } finally {
    testObject.close()
  }
})
