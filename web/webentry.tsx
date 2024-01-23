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
  outputSourceLocation,
  TokenRoot,
  SourceLocation,
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

import * as Popover from '@radix-ui/react-popover';
import * as Immer from 'immer'

Immer.setAutoFreeze(false) // This breaks stuff otherwise, because we put the compiler state in immer

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
              {
                type: 'tab',
                name: 'Timeline',
                component: 'TimelineView',
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
  font-family: arial;
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
.scrolly {
  overflow-y: auto;
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
.inspect-error {
  color: red;
}
.inspect--clickable {
  cursor: default;
}
.inspect--clickable:hover {
  text-decoration: underline;
}
button {
  font-size: inherit;
  font-family: inherit;
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
.popover-content {
  background: #333;
  padding: 8px;
  box-shadow: 10px 10px 52px -29px rgba(0,0,0,0.75);
  min-width: 300px;
}
.flexlayout__tab_button_content {
  user-select: none;
}
.timelinetask:hover {
  outline: 2px solid white;
  cursor: default;
}
.timelinelabel {
  font-size: 10px;
  box-sizing: border-box;
  margin: 1px;
  padding: 2px;
  pointerEvents: none;
  white-space: nowrap;
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

const loadFile = async (file: string) => {
  const content = await fetch(`/fixtures/${file}`).then(x => x.text())
  store.update({
    currentTask: null,
    stepIndex: 0,
    inspectKey: [],
    inspectObject: null,
    input: content,
    testRunner: null
  })
  logs.length = 0
}

const LoadMenu = ({ trigger }) => {
  const [open, setOpen] = React.useState(false)
  return (
    <Popover.Root open={open} onOpenChange={setOpen}>
      <Popover.Trigger asChild>{trigger}</Popover.Trigger>
      <Popover.Portal>
        <Popover.Content
          className="popover-content"
          sideOffset={5}
        >
          <div class="flex-column scrolly" style={{height: 300}}>
          {globalThis.FILES.map(file => {
            return <button class="button" style={{textAlign: 'left'}} onClick={() => {loadFile(file); setOpen(false)}}>{file}</button>
          })}
          </div>
        </Popover.Content>
      </Popover.Portal>
    </Popover.Root>
  )
}

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
    } else if (component === 'TimelineView') {
      return <TimelineView />
    } else if (component === 'InspectView') {
      return <InspectView />
    } else if (component === 'FunctionsView') {
      return <FunctionsView />
    }
  }

  return <div class='flex-column pin'>
    <div class='flex-row'>
      <LoadMenu trigger={<button className="button button--thick">Load</button>} />
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
  const testRunner = store.useValue((x) => x.testRunner)
  if (!testRunner) return;

  return (
    <div class="monospace scrollx h-full">
      {inspect(testRunner.queue.currentTask, { depth: 1 })}
    </div>
  )
}
const InputView = () => {
  const input = store.useValue((x) => x.input)
  return (
    <div>
      <EnhancedTextArea text={input} onTextChange={updateInput} />
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
  const testRunner = store.useValue((x) => x.testRunner)
  if (!testRunner) return;
  const compiler = testRunner.compiler

  const entries = Array.from(compiler.compiledFunctions.entries())
  const value = Object.fromEntries(entries.map(x => [x[0].name, x[1]]))
  const frags = inspect(value, { depth: 2, space: 20 })

  return <div class="monospace scrollx h-full">{frags}</div>
}


const timeline: any[][] = []
const updateTimeline = (currentTask: Task<unknown, unknown>) => {
  
  const v: any[] = []
  timeline.push(v)
  
  if (currentTask) {
    let task = currentTask

    while (task) {
      v.push(task);
      const dep = task._dependant ?? task._prevInChain
      task = dep
    }
  }
  v.reverse()
  console.log(timeline)
}

const makeTree = (rootTask, events, width) => {
  if (!store.state.testRunner) return;

  const taskDivs: any[] = []
  const taskLabels: any[] = []
  const queue = store.state.testRunner.queue

  let max = 0;
  let maxTicks = timeline.length

  const itemWidth = Math.max((width) / maxTicks, 2)
  
  const div = (task, startTick, top, width) => {
    const background = task._state === 'started' ? 'blue' : task._state === 'completed' ? 'green' : "red"
    const div = <div 
      {...events(task)}
      class="timelinetask"
      style={{ position: 'absolute',
        left: startTick * itemWidth,
        top: top * 15,
        height: 14,
        width: (width * itemWidth) - 1,
        fontSize: 10,
        boxSizing: "border-box",
          color: 'white', margin: 1, background, padding: 2 }}
      ></div>
    const divLabel = <div class="timelinelabel"
      style={{ position: 'absolute',
        left: startTick * itemWidth,
        top: top * 15 }}
      >{task.id}{task.def ? ` ${task.def}` : null}</div>
    taskDivs.push(div)
    taskLabels.push(divLabel)
  }

  let i = 0;
  for (const v of timeline) {
    max = Math.max(max, v.length)
    let j = 0;
    for (const task of v) {
      let before = timeline[i - 1] && timeline[i - 1][j] === task;
      if (!before) {
        let width = 1;
        for (let w = i+1; w < timeline.length && timeline[w][j] === task; w++) {
          width ++
        }
        div(task, i, j, width)
      }
      j ++;
    }
    i++
  }
  
  return <div style={{position:'relative', width: itemWidth * maxTicks, height: max * 20, overflowX: 'hidden'}}>
    {taskDivs}
    {taskLabels}
  </div>

}

const TimelineView = () => {
  const stepIndex = store.useValue((x) => x.stepIndex)
  const [s, setS] = React.useState(0)
  const ref = React.useRef()

  const createEvents = (task) => {
    return { onClick: () => clickInspect(task, [], false) } 
  }
  React.useEffect(() => {
    if (!ref.current) setS(s => s + 1)
  }, [])
  if (!ref.current) return <div ref={ref}></div>

  return <div ref={ref}>{makeTree(null, createEvents, ref.current.offsetWidth - 10)}</div>
}

const runTestInner = (input: string, { onStep }) => {
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
    if (queue.list.length === 0) return
    while (true) {
      stepQueue(queue)
      onStep(queue.currentTask)
      if ((queue.currentTask as any).def) break
    }
  }
  function runToEnd() {
    while (queue.list.length) { stepQueue(queue); onStep(queue.currentTask) }
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

const initialInput = `

import my_module for foo

fn main():
  foo(200)


`

const store = createStore({
  currentTask: null as Task<unknown, unknown> | null,
  stepIndex: 0,
  inspectObject: null as unknown,
  inspectKey: [] as string[],
  testRunner: null as unknown,
  input: initialInput,
  error: null as any
})


const onStep = (currentTask: Task<unknown, unknown>) => {
  console.log("1 step")
  updateTimeline(currentTask)
}
const getOrCreateTestRunner = () => {
  let testRunner: any = store.state.testRunner
  if (!testRunner) {
    testRunner = runTestInner(store.state.input, { onStep })
    store.update({ testRunner })
    logs.length = 0
    timeline.length = 0
  }
  return testRunner
}
const stepUi = () => {
  const testRunner = getOrCreateTestRunner()
  let error = null
  try {
    testRunner.step()
  } catch(ex) {
    error = ex
    console.error(ex)
    logError(ex)
  }
  store.update({ error, currentTask: testRunner.queue.currentTask, stepIndex: store.state.stepIndex + 1 })
}
const runToEndUi = () => {
  const testRunner = getOrCreateTestRunner()
  let error = null
  try {
    testRunner.runToEnd()
  } catch(ex) {
    error = ex
    console.error(ex)
    logError(ex)
  }
  store.update({ error, currentTask: testRunner.queue.currentTask, stepIndex: store.state.stepIndex + 1 })
}
const updateInput = (input: string) => {
  store.update({ input, testRunner: null })
}
const logError = (ex: Error) => {
  if (ex instanceof Error) {
    // if (ex.stack) logger.log(ex.stack)
    logger.log(<span class='inspect-error'>{ex.toString()}</span>)
  }
  if (ex instanceof CompilerError) {
    const location = (ex.info as any).location as SourceLocation
    if (location) {
      const text = outputSourceLocation(location)
      logger.log(text)
    }

    if ((ex.info as any)._userinfo) {
      ;(ex.info as any)._userinfo.forEach((name) => {
        const item = ex.info[name]
        if (item && Object.getPrototypeOf(item) === TokenRoot) {
          const text = outputSourceLocation(item.location)
          logger.log(text)
        }
      })
    }

    logger.log('\nError info')
    const o = Object.fromEntries(Object.entries(ex.info))
    logger.log(inspect(o, { depth: 1}))
    
  }
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
      obj.forEach((item, i) => {
        if (i !== 0) frags.push(", ")
        frags.push(inspect(item, {...subOptions, key: keyed(i)}))
      })
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
