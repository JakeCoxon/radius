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
        width: 400,
        // weight: 50,
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
  background: #0d0d0d;
  color: #eee;

}
code {
  white-space: pre;
  font-family: monospace;
}
.flex-row {
  display: flex;
  flex-direction: row;
}
.flex-column {
  display: flex;
  flex-direction: column;
}
.pin {
  position: absolute;
  left: 0; right: 0;
  top: 0; bottom: 0;
}

.flexlayout__layout {
  --color-background: transparent;
  --color-tabset-background: transparent;
  --color-tabset-background-selected: transparent;
  --color-splitter: black;
  --color-tabset-divider-line: black;
}

.flexlayout__tabset-selected {
  background-color: var(--color-tabset-background-selected);
  background-image: none;
}
.monospace {
  font-family: monospace;
}
.scrollx {
  overflow-x: auto;
  white-space: nowrap;
}
.h-full {
  height: 100%;
}
.inspect-object {
  color: #0ea5e9;
}
.inspect-string {
  color: #10b981;
}
.inspect-number {
  color: #eab308;
}
.inspect--clickable {
  cursor: default;
}
.inspect--clickable:hover {
  text-decoration: underline;
}
button {
  font-size: inherit;
}
.button {
  border: 0;
  background: #333;
  margin: 2px;
  color: #ccc;
  border-radius: 2px;
}
.button:hover {
  background: #444;
}
.button--thick {
  padding: 4px 8px;
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

  return <div class='flex-column pin'>
    <div class='flex-row'>
      <button class="button button--thick" onClick={stepUi}>Step</button>
      <button class="button button--thick" onClick={runToEndUi}>Compile</button>
    </div>
    <div style={{position:'relative', flexGrow:1}}>
      <Layout model={model} factory={factory} />
    </div>
  </div>
}
const TaskView = () => {
  const task = store.useValue((x) => x.currentTask)
  return (
    <div class="monospace scrollx h-full">
      {inspect(queue.currentTask, { depth: 1 })}
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
  const inspectObject = store.useValue(x => x.inspectObject)
  const inspectKey = store.useValue(x => x.inspectKey)

  if (inspectObject === null || inspectObject === undefined) return <div></div>

  const finalObj = inspectKey.reduce((acc, x) => acc[x], inspectObject)

  return <div class="monospace scrollx h-full">
    <div style={{marginBottom:4, color: '#666'}}>
      <small>
        <button class="button" onClick={() => clickInspectUp()}>Up</button>
        {['inspect', ...inspectKey].join(".")}</small></div>
    <div>{inspect(finalObj, { depth: 1, relativeView: true })}</div>
  </div>
}

const LogView = () => {
  const stepIndex = store.useValue((x) => x.stepIndex)

  const frags: any[] = []
  for (const log of logs) {
    for (const item of log) {
      frags.push(<div><code>{item}</code></div>)
    }
  }

  return <div class="monospace">{frags}</div>
}
const FunctionsView = () => {
  const stepIndex = store.useValue((x) => x.stepIndex)

  const entries = Array.from(compiler.compiledFunctions.entries())
  const value = Object.fromEntries(entries.map(x => [x[0].name, x[1]]))
  const frags = inspect(value, { depth: 2, space: 20 })

  return <div class="monospace scrollx h-full">{frags}</div>
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
  subCompilerState.moduleCompiler = subCompilerState
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
  function runToEnd() {
    while (queue.list.length) { stepQueue(queue) }
    end()
  }
  function end() {
    if (root._state !== 'completed') {
      // TODO: remove events after they are completed
      globalCompiler.allWaitingEvents.forEach((e) => e.failure({}))
    }
    compilerAssert(root._success, 'Expected success', { root })
  }

  return { compiler: globalCompiler, step, queue, runToEnd }
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

const { compiler, step, runToEnd, queue } = runTestInner(input)
const store = createStore({
  currentTask: null as Task<unknown, unknown> | null,
  stepIndex: 0,
  inspectObject: null as unknown,
  inspectKey: [] as string[]
})

const stepUi = () => {
  step()
  store.update({ currentTask: queue.currentTask, stepIndex: store.state.stepIndex + 1 })
}
const runToEndUi = () => {
  runToEnd()
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

const clickInspect = (obj: unknown, key: string[], relativeView: boolean) => {
  // console.log(obj)
  console.log("key", key)
  if (relativeView) {
    store.update(s => s.inspectKey.push(...key))
  } else {
    store.update({ inspectObject: obj, inspectKey: [] })
  }
}
const clickInspectUp = () => {
  store.update(s => s.inspectKey.pop())
}

const inspect = (obj: unknown, options = {}) => {
  const key = options.key || []
  const keyed = (str: unknown) => [...key, str]
  const subOptions = { depth: options.depth - 1, relativeView: options.relativeView }

  if (typeof obj === 'string') return <span class="inspect-string">"{obj}"</span>
  if (typeof obj === 'number') return <span class="inspect-number">{obj}</span>
  if (typeof obj === 'boolean') return <span class="inspect-boolean">{String(obj)}</span>
  if (obj === null) return `null`
  if (obj === undefined) return `undefined`
  if (options.depth <= 0) {
    if (Array.isArray(obj)) {
      const frags: any = []
      frags.push(`[`)
      frags.push(...obj.map((item, i) => inspect(item, {...subOptions, key: keyed(i)})))
      frags.push(`]`)
      return frags 
    }
    if (obj[Inspect.custom]) {
      const item = obj[Inspect.custom](0, { stylize: (x) => x }, subOptions)
      return <span class="inspect-object inspect--clickable" onClick={() => clickInspect(obj, key, options.relativeView)}>{item}</span>
    }
    const name = obj.constructor?.name || "Object"
    return <span class="inspect-object inspect--clickable" onClick={() => clickInspect(obj, key, options.relativeView)}>[{name}]</span>
  }
  if (typeof obj !== 'object') return `??`

  const frags: any[] = []
  Object.entries(obj).forEach(([key, value], i) => {
    if (options.space && i !== 0) frags.push(<div style={{height: options.space}}></div>)

    const valueObj = inspect(value, {...subOptions, key: keyed(key) })

    if (options.depth == 1) {
      frags.push(<div class="flex-row"><div style={{width:150, flexShrink: 0}}>{key}</div>
        {valueObj}</div>)
    } else {
      frags.push(<div><div>{key}</div>{valueObj}</div>)
    }
  })
  return frags
}
