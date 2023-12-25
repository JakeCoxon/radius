import { test, expect, describe } from "bun:test";
import { createModuleLoader, runCompilerTest } from "./testUtils";

test("basic", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/basic.rad`).text()
  const test = runCompilerTest(input, { filename: 'basic' })

});

test("closure", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/closure.rad`).text()
  const test = runCompilerTest(input, { filename: 'closure' })

});

test("closure2", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/closure2.rad`).text()
  const test = runCompilerTest(input, { filename: 'closure2' })

});

test("closure_binding", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/closure_binding.rad`).text()
  const test = runCompilerTest(input, { filename: 'closure_binding' })

});


test("comptime", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/comptime.rad`).text()
  const test = runCompilerTest(input, { filename: "comptime" })

  expect(test.prints).toEqual(
   [
     5,
     "\"thing\"",
     12,
     44,
     1850,
     "\"thing\"",
     12,
     44,
     44,
     "\"thing\"",
     12,
     44
   ]
  )


  
});

test("Expressions", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/expressions.rad`).text()
  const test = runCompilerTest(input, { filename: 'expressions' })

});

test("Identifier error", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/identifier_error.rad`).text()
  const test = runCompilerTest(input, { filename: 'identifier_error', expectError: true })

});


test("Identifier error2", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/identifier_error2.rad`).text()
  const test = runCompilerTest(input, { filename: 'identifier_error2', expectError: true })

});

test("list_iterator", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/list_iterator.rad`).text()
  const test = runCompilerTest(input, { filename: 'list_iterator' })

});

test("custom_iterator", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/custom_iterator.rad`).text()
  const test = runCompilerTest(input, { filename: 'custom_iterator' })

});


test("noclosure", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/noclosure.rad`).text()
  const test = runCompilerTest(input, { filename: 'noclosure', expectError: true })

});

test.todo("noshadow", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/noshadow.rad`).text()
  const test = runCompilerTest(input, { filename: "noshadow" })

  // These should be the same
  expect(test.prints).toEqual([
    100,
    100
  ])

  
});

test("struct", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/struct.rad`).text()
  runCompilerTest(input, { filename: 'struct' })
  
});

test("random", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/random.rad`).text()
  const test = runCompilerTest(input, { filename: 'random' })

});

test("module", async () => {

  const moduleLoader = createModuleLoader(`${import.meta.dir}/fixtures/imports/`)
  const input = await Bun.file(`${import.meta.dir}/fixtures/module.rad`).text()
  const test = runCompilerTest(input, { moduleLoader, filename: 'module' })

});

test("methods", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/methods.rad`).text()
  const test = runCompilerTest(input, { filename: 'methods' })

});

test("nomethod", async () => {

  const input = await Bun.file(`${import.meta.dir}/fixtures/nomethod.rad`).text()
  const test = runCompilerTest(input, { filename: 'nomethod', expectError: true })

});
