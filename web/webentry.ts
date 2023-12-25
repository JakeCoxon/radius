import { runTopLevelTask } from "../src/compiler";
import { BoolType, Closure, CompilerError, DoubleType, ExternalFunction, FloatType, GlobalCompilerState, IntType, Scope, StringType, SubCompilerState, TaskContext, VoidType, compilerAssert, createDefaultGlobalCompiler, createScope, expectMap, BuiltinTypes } from "../src/defs";
import { makeParser } from "../src/parser"
import { Queue, TaskDef, stepQueue, withContext } from "../src//tasks";
import { functionTemplateTypeCheckAndCompileTask } from "../src/compiler_functions";

const runTestInner = (input: string) => {
  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.logger = logger
  
  const rootScope: Scope = createScope({
    ...BuiltinTypes,
    compfoo: { _function: (a, b) => 65 + a + b },
    bar: 123,
    print: new ExternalFunction('print', VoidType, (...args) => {
      console.log(...args)
      return args[0];
    }),
  }, undefined);

  const queue = new Queue();
  
  const parser = makeParser(input, 'main')
  
  const subCompilerState = new SubCompilerState('root');
  subCompilerState.scope = rootScope
  const root = (
    TaskDef(runTopLevelTask, parser.rootNode, rootScope)
    .chainFn((task, arg) => {
      const func: Closure = expectMap(rootScope, "main", "No main function found");
      return TaskDef(functionTemplateTypeCheckAndCompileTask, { func: func.func, args: [], typeArgs: [], parentScope: func.scope, concreteTypes: [] })
    })
    .wrap(withContext({ globalCompiler, subCompilerState } as TaskContext))
  );
  queue.enqueue(root)

  function step() {
    if (queue.list.length === 0) end()
    stepQueue(queue);
  }
  function end() {
    if (root._state !== 'completed') {
      // TODO: remove events after they are completed
      globalCompiler.allWaitingEvents.forEach(e => e.failure({}))
    }
    compilerAssert(root._success, "Expected success", { root })
  }

  return { compiler: globalCompiler, step, queue }
  
}

const logs: any[][] = []

const logger = { log: (...args) => {
  logs.push(args)
} }





const input = `

fn main():
  thing(3, 2, 1)
  print("Hello")

fn thing(x: int, y: int, z: int):
  print(x + y * z)

`

const { compiler, step, queue } = runTestInner(input)

const STYLE = `
body {
  background: #111;
  color: #eee;
}
code {
  white-space: pre;
  font-family: monospace;
}

`

document.addEventListener('DOMContentLoaded', () => {
  const style   = ((child) => {document.body.appendChild(child); return child})(document.createElement('style'))
  const div     = ((child) => {document.body.appendChild(child); return child})(document.createElement('div'))
  const button  = ((child) => {div.appendChild(child); return child})(document.createElement('button'))
  const content = ((child) => {div.appendChild(child); return child})(document.createElement('code'))
  style.innerText = STYLE

  Object.assign(button, { innerText: "step" })

  button.addEventListener('click', () => {
    step();
    console.log(queue.currentTask)


    let text = ''
    text += `${inspect(queue.currentTask, { depth: 1 })}\n`
    

    text += `Functions:\n`;
    [...compiler.compiledFunctions.entries()].forEach(([key, value]) => {
      text += `${key}: ${inspect(value, { depth: 1 })}\n`
    });

    text += `\n\n`;
    text += `Logs: \n`;
    for (const log of logs) {
      for (const item of log) {
        text += inspect(item) + " "
      }
      text += `\n`
    }

    // text += `Classes: `;
    // [...compiler.classDefinitions.entries()].forEach((key, value) => {
    //   text += `${key}\n`
    // })

    content.innerHTML  = text
  })



  console.log("Hello")
})

const inspect = (obj, options={}) => {
  if (typeof obj === 'string') return obj;
  if (typeof obj === 'number') return obj
  if (obj === null) return `null`
  if (obj === undefined) return `undefined`
  if (options.depth <= 0) return `[Object]`
  if (typeof obj !== 'object') return `??`

  let text = ''
  for (const [key, value] of Object.entries(obj)) {
    text += `  ${key}: ${inspect(value, { depth: options.depth - 1 })}\n`
  }
  return text
}