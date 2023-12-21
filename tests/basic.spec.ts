import { test, expect, describe } from "bun:test";
import { runCompilerTest } from "./testUtils";

test("basic", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/basic.rad`).text()
  const test = runCompilerTest(input, { filename: 'basic' })

});

test("closure", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/closure.rad`).text()
  const test = runCompilerTest(input, { filename: 'closure' })

});


test("Comptime", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/comptime.rad`).text()
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

  const input = await Bun.file(`${__dirname}/fixtures/expressions.rad`).text()
  const test = runCompilerTest(input, { filename: 'expressions' })

});

test("Identifier error", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/identifier_error.rad`).text()
  const test = runCompilerTest(input, { filename: 'identifier_error', expectError: true })

});


test("Identifier error2", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/identifier_error2.rad`).text()
  const test = runCompilerTest(input, { filename: 'identifier_error2', expectError: true })

});

test("list_iterator", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/list_iterator.rad`).text()
  const test = runCompilerTest(input, { filename: 'list_iterator' })

});


test("noclosure", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/noclosure.rad`).text()
  const test = runCompilerTest(input, { filename: 'noclosure', expectError: true })

});

test.todo("noshadow", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/noshadow.rad`).text()
  const test = runCompilerTest(input, { filename: "noshadow" })

  // These should be the same
  expect(test.prints).toEqual([
    100,
    100
  ])

  
});

test("struct", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/struct.rad`).text()
  runCompilerTest(input, { filename: 'struct' })
  
});

test("random", async () => {

  const input = await Bun.file(`${__dirname}/fixtures/random.rad`).text()
  const test = runCompilerTest(input, { filename: 'random' })

});
