import { test, expect, describe } from 'bun:test'
import { createModuleLoader, createTest } from './testUtils'
import { GlobalExternalCompilerOptions } from '../src/defs'

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
  await testObject.run()
  testObject.close()
})

test('match', async () => {
  const testObject = createTest({ 
    moduleName: 'match',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/match.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('option', async () => {
  const testObject = createTest({ 
    moduleName: 'option',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/option.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('guard', async () => {
  const testObject = createTest({ 
    moduleName: 'guard',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/guard.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('option_expansion', async () => {
  const testObject = createTest({ 
    moduleName: 'option_expansion',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/option_expansion.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('basicasdf', async () => {
  const testObject = createTest({ 
    moduleName: 'basic',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/basic.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('new_array', async () => {
  const testObject = createTest({ 
    moduleName: 'new_array',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/new_array.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('lib_array', async () => {
  const testObject = createTest({ 
    moduleName: 'lib_array',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/lib_array.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('subscript', async () => {
  const testObject = createTest({ 
    moduleName: 'subscript',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/subscript.rad`,
  })
  await testObject.run()
  testObject.close()
})


test('closure', async () => {
  const testObject = createTest({ 
    moduleName: 'closure',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('closure2', async () => {
  const testObject = createTest({ 
    moduleName: 'closure2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure2.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('closure_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_binding',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_binding.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('closure_closed_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_closed_binding',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_closed_binding.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('closure_closed_inline_binding', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_closed_inline_binding',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_closed_inline_binding.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('closure_compose', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_compose',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_compose.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('closure_escape', async () => {
  const testObject = createTest({ 
    moduleName: 'closure_escape',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/closure_escape.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('inline', async () => {
  const testObject = createTest({ 
    moduleName: 'inline',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/inline.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('inline_return', async () => {
  const testObject = createTest({ 
    moduleName: 'inline_return',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/inline_return.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('inline_shadow', async () => {
  const testObject = createTest({ 
    moduleName: 'inline_shadow',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/inline_shadow.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('comptime', async () => {
  const testObject = createTest({ 
    moduleName: 'comptime',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/comptime.rad`,
  })
  await testObject.run()
  testObject.close()
})

test.todo('expressions', async () => {
  const testObject = createTest({ 
    moduleName: 'expressions',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/expressions.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('identifier_error', async () => {
  const testObject = createTest({ 
    moduleName: 'identifier_error',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/identifier_error.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('identifier_error2', async () => {
  const testObject = createTest({ 
    moduleName: 'identifier_error2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/identifier_error2.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('list_iterator', async () => {
  const testObject = createTest({ 
    moduleName: 'list_iterator',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/list_iterator.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('custom_iterator', async () => {
  const testObject = createTest({ 
    moduleName: 'custom_iterator',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/custom_iterator.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('noclosure', async () => {
  const testObject = createTest({ 
    moduleName: 'noclosure',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/noclosure.rad`,
  })
  await testObject.run()
  testObject.close()
})

test.todo('noshadow', async () => {
  const testObject = createTest({ 
    moduleName: 'noshadow',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/noshadow.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('struct', async () => {
  const testObject = createTest({ 
    moduleName: 'struct',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/struct.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('random', async () => {
  const testObject = createTest({ 
    moduleName: 'random',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/random.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('module', async () => {
  const testObject = createTest({ 
    moduleName: 'module',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/module.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('methods', async () => {
  const testObject = createTest({ 
    moduleName: 'methods',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/methods.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('methods2', async () => {
  const testObject = createTest({ 
    moduleName: 'methods2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/methods2.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('nomethod', async () => {
  const testObject = createTest({ 
    moduleName: 'nomethod',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/nomethod.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('template_basic', async () => {
  const testObject = createTest({ 
    moduleName: 'template_basic',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/template_basic.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('template_advanced', async () => {
  const testObject = createTest({ 
    moduleName: 'template_advanced',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/template_advanced.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('list_struct', async () => {
  const testObject = createTest({ 
    moduleName: 'list_struct',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/list_struct.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('generic', async () => {
  const testObject = createTest({ 
    moduleName: 'generic',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/generic.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('generic2', async () => {
  const testObject = createTest({ 
    moduleName: 'generic2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/generic2.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('tuple', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/tuple.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('tuple_extract', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple_extract',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/tuple_extract.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('tuple_return_type', async () => {
  const testObject = createTest({ 
    moduleName: 'tuple_return_type',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/tuple_return_type.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('dict', async () => {
  const testObject = createTest({ 
    moduleName: 'dict',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/dict.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('dict2', async () => {
  const testObject = createTest({ 
    moduleName: 'dict2',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/dict2.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('transduce', async () => {
  const testObject = createTest({ 
    moduleName: 'transduce',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/transduce.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('list', async () => {
  const testObject = createTest({ 
    moduleName: 'list',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/list.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('range2d', async () => {
  const testObject = createTest({ 
    moduleName: 'range2d',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/range2d.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('vec', async () => {
  const testObject = createTest({ 
    moduleName: 'vec',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/vec.rad`,
  })
  await testObject.run()
  testObject.close()
})
test('named_break', async () => {
  const testObject = createTest({ 
    moduleName: 'named_break',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/named_break.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('ifs', async () => {
  const testObject = createTest({ 
    moduleName: 'ifs',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/ifs.rad`,
  })
  await testObject.run()
  testObject.close()
})
test('expansion', async () => {
  const testObject = createTest({ 
    moduleName: 'expansion',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/expansion.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('meta', async () => {
  const testObject = createTest({ 
    moduleName: 'meta',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/meta.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('meta_class', async () => {
  const testObject = createTest({ 
    moduleName: 'meta_class',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/meta_class.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('advanced', async () => {
  const testObject = createTest({ 
    moduleName: 'advanced',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/advanced.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('reftype', async () => {
  const testObject = createTest({ 
    moduleName: 'reftype',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/reftype.rad`,
  })
  await testObject.run()
  testObject.close()
})
test('valtype', async () => {
  const testObject = createTest({ 
    moduleName: 'valtype',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/valtype.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('returns', async () => {
  const testObject = createTest({ 
    moduleName: 'returns',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/returns.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('global', async () => {
  const testObject = createTest({ 
    moduleName: 'global',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/global.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('compiler_module', async () => {
  const testObject = createTest({ 
    moduleName: 'compiler_module',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/compiler_module.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('numbers', async () => {
  const testObject = createTest({ 
    moduleName: 'numbers',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/numbers.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('ops', async () => {
  const testObject = createTest({ 
    moduleName: 'ops',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/ops.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('binding_bug', async () => {
  const testObject = createTest({ 
    moduleName: 'binding_bug',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/binding_bug.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('current_loop', async () => {
  const testObject = createTest({ 
    moduleName: 'current_loop',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/current_loop.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('print', async () => {
  const testObject = createTest({ 
    moduleName: 'print',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/print.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('add_operator_bug', async () => {
  const testObject = createTest({ 
    moduleName: 'add_operator_bug',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/add_operator_bug.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('move_result', async () => {
  const testObject = createTest({ 
    moduleName: 'move_result',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/move_result.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('block_projection', async () => {
  const testObject = createTest({ 
    moduleName: 'block_projection',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/block_projection.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('overload', async () => {
  const testObject = createTest({ 
    moduleName: 'overload',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/overload.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('named_args', async () => {
  const testObject = createTest({ 
    moduleName: 'named_args',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/named_args.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('overload_error', async () => {
  const testObject = createTest({ 
    moduleName: 'overload_error',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/overload_error.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('generator', async () => {
  const testObject = createTest({ 
    moduleName: 'generator',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/generator.rad`,
  })
  await testObject.run()
  testObject.close()
})

test('iterator_expansion', async () => {
  const testObject = createTest({ 
    moduleName: 'iterator_expansion',
    globalOptions,
    inputPath: `${import.meta.dir}/fixtures/iterator_expansion.rad`,
  })
  await testObject.run()
  testObject.close()
})
