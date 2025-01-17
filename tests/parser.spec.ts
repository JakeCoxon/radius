import { test, expect, describe } from 'bun:test'

import { makeParser } from '../src/parser'

test('parser', async () => {
  const inputPath = `${import.meta.dir}/fixtures/parser.rad`
  try {
    const input = await Bun.file(inputPath).text()
    const parser = makeParser(input, "parser")
  } catch(ex) {
    
    throw ex
  } finally {
  }
})