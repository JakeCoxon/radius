import { runTopLevelTask } from '../src/compiler'
import {
  BoolType,
  Closure,
  CompilerError,
  DoubleType,
  ExternalFunction,
  FloatType,
  GlobalCompilerState,
  IntType,
  Scope,
  StringType,
  SubCompilerState,
  TaskContext,
  VoidType,
  compilerAssert,
  createDefaultGlobalCompiler,
  createScope,
  expectMap,
  BuiltinTypes,
  Inspect,
} from '../src/defs'
import { makeParser } from '../src/parser'
import { Queue, Task, TaskDef, stepQueue, withContext } from '../src/tasks'
import { functionTemplateTypeCheckAndCompileTask } from '../src/compiler_functions'

import React from 'react'
import { createRoot } from 'react-dom/client'

import { Layout, Model } from 'flexlayout-react'
import css from 'flexlayout-react/style/dark.css'
import { createStore } from './pureStore'
import { EnhancedTextArea } from './Textarea'

const layoutConfig = {
  global: {
    splitterSize: 4,
  },
  borders: [],
  layout: {
    type: 'row',
    weight: 100,
    children: [
      {
        type: 'tabset',
        weight: 50,
        children: [
          {
            type: 'tab',
            name: 'input',
            component: 'InputView',
          },
        ],
      },
      {
        type: 'row',
        weight: 100,
        children: [
          {
            type: 'row',
            weight: 100,
            children: [
              {
                type: 'tabset',
                weight: 50,
                children: [
                  {
                    type: 'tab',
                    name: 'Task',
                    component: 'TaskView',
                  },
                ],
              },
              {
                type: 'tabset',
                weight: 50,
                children: [
                  {
                    type: 'tab',
                    name: 'Functions',
                    component: 'FunctionsView',
                  },
                ],
              },
              {
                type: 'tabset',
                weight: 50,
                children: [
                  {
                    type: 'tab',
                    name: 'Inspect',
                    component: 'InspectView',
                  },
                ],
              },
            ],
          },
          {
            type: 'tabset',
            weight: 30,
            children: [
              {
                type: 'tab',
                name: 'Log',
                component: 'LogView',
              },
            ],
          },
        ],
      },
    ],
  },
}

const STYLE = `
body {
  background: #111;
  color: #eee;
}
code {
  white-space: pre;
  font-family: monospace;
}

.flexlayout__tabset-selected {
  background-color: var(--color-tabset-background-selected);
  background-image: none;
}

textarea {
  background: #111;
  color: #fff;
  border: 0;
  width: 100%;
  height: 100%;
  min-height: 500px;
}

`

// Clear the existing HTML content
document.body.innerHTML = `<link rel="stylesheet" href="${css}" /><style>${STYLE}</style><div id="app"></div>`

// Render your React component instead
const root = createRoot(document.getElementById('app'))
root.render(<App />)

const model = Model.fromJson(layoutConfig)

function App() {
  const factory = (node) => {
    var component = node.getComponent()

    if (component === 'button') {
      return <button>{node.getName()}</button>
    } else if (component === 'TaskView') {
      return <TaskView />
    } else if (component === 'InputView') {
      return <InputView />
    } else if (component === 'LogView') {
      return <LogView />
    } else if (component === 'InspectView') {
      return <InspectView />
    } else if (component === 'FunctionsView') {
      return <FunctionsView />
    }
  }

  return <Layout model={model} factory={factory} />
}
const TaskView = () => {
  const task = store.useValue((x) => x.currentTask)
  return (
    <div>
      <div>
        <button onClick={stepUi}>Step</button>
      </div>
      <pre>{`${inspect(queue.currentTask, { depth: 1 })}\n`}</pre>
    </div>
  )
}
const InputView = () => {
  return (
    <div>
      <EnhancedTextArea initialText={input} />
    </div>
  )
}
const InspectView = () => {
  return <div>inspect</div>
}
const LogView = () => {
  const stepIndex = store.useValue((x) => x.stepIndex)

  let text = ''

  text += `\n\n`
  text += `Logs: \n`
  for (const log of logs) {
    for (const item of log) {
      text += inspect(item) + ' '
    }
    text += `\n`
  }

  return <pre>{text}</pre>
}
const FunctionsView = () => {
  const stepIndex = store.useValue((x) => x.stepIndex)

  let text = ''
  Array.from(compiler.compiledFunctions.entries()).forEach(([key, value]) => {
    text += `${key}: ${inspect(value, { depth: 1 })}\n`
  })

  return <pre>{text}</pre>
}

const runTestInner = (input: string) => {
  const globalCompiler = createDefaultGlobalCompiler()
  globalCompiler.logger = logger
  globalCompiler.moduleLoader = {
    cache: {},
    loadModule: (module) => {
      return makeParser(module1, module)
    },
  }

  const rootScope: Scope = createScope(
    {
      ...BuiltinTypes,
      compfoo: { _function: (a, b) => 65 + a + b },
      print: new ExternalFunction('print', VoidType, (...args) => {
        console.log(...args)
        return args[0]
      }),
    },
    undefined
  )

  const moduleScope = Object.create(rootScope)

  const queue = new Queue()

  const parser = makeParser(input, 'main')

  const subCompilerState = new SubCompilerState('moduleScope')
  subCompilerState.globalCompiler = globalCompiler
  subCompilerState.scope = moduleScope
  const root = TaskDef(runTopLevelTask, parser.rootNode, rootScope, moduleScope)
    .chainFn((task, arg) => {
      const func: Closure = expectMap(moduleScope, 'main', 'No main function found')
      return TaskDef(functionTemplateTypeCheckAndCompileTask, {
        func: func.func,
        args: [],
        typeArgs: [],
        parentScope: func.scope,
        concreteTypes: [],
      })
    })
    .wrap(withContext({ globalCompiler, subCompilerState } as TaskContext))
  queue.enqueue(root)

  function step() {
    if (queue.list.length === 0) end()
    while (true) {
      stepQueue(queue)
      if ((queue.currentTask as any).def) break
    }
  }
  function end() {
    if (root._state !== 'completed') {
      // TODO: remove events after they are completed
      globalCompiler.allWaitingEvents.forEach((e) => e.failure({}))
    }
    compilerAssert(root._success, 'Expected success', { root })
  }

  return { compiler: globalCompiler, step, queue }
}

const logs: any[][] = []

const logger = {
  log: (...args) => {
    logs.push(args)
  },
}

const module1 = `
fn foo(x: int):
  print(x)

fn foo2(x: int):
  print(x)
`

const input = `

import my_module for foo

fn main():
  foo(200)


`

const { compiler, step, queue } = runTestInner(input)
const store = createStore({
  currentTask: null as Task<unknown, unknown> | null,
  stepIndex: 0,
})

const stepUi = () => {
  step()
  store.update({ currentTask: queue.currentTask, stepIndex: store.state.stepIndex + 1 })
}

// document.addEventListener('DOMContentLoaded', () => {
//   const style   = ((child) => {document.body.appendChild(child); return child})(document.createElement('style'))
//   const div     = ((child) => {document.body.appendChild(child); return child})(document.createElement('div'))
//   const button  = ((child) => {div.appendChild(child); return child})(document.createElement('button'))
//   const content = ((child) => {div.appendChild(child); return child})(document.createElement('code'))
//   style.innerText = STYLE

//   Object.assign(button, { innerText: "step" })

//   button.addEventListener('click', () => {
//     step();
//     console.log(queue.currentTask)

//     let text = ''
//     text += `${inspect(queue.currentTask, { depth: 1 })}\n`

//     text += `Functions:\n`;
//     Array.from(compiler.compiledFunctions.entries()).forEach(([key, value]) => {
//       text += `${key}: ${inspect(value, { depth: 1 })}\n`
//     });

//     text += `\n\n`;
//     text += `Logs: \n`;
//     for (const log of logs) {
//       for (const item of log) {
//         text += inspect(item) + " "
//       }
//       text += `\n`
//     }

//     // text += `Classes: `;
//     // [...compiler.classDefinitions.entries()].forEach((key, value) => {
//     //   text += `${key}\n`
//     // })

//     content.innerHTML  = text
//   })

//   console.log("Hello")
// })

const inspect = (obj, options = {}) => {
  if (typeof obj === 'string') return obj
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
