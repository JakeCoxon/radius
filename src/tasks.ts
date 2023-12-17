
let uniqueId = 1000;
export function resetTaskIds() {
  uniqueId = 1000;
}

export type Cancel = () => void;

// export type Queue = {
//   tick: number,
//   currentTask: Task<unknown, unknown>,
//   final: Task<unknown, unknown>,
//   list: Task<unknown, unknown>[],
//   completed: Task<unknown, unknown>[],
//   enqueue: (task: Task<unknown, unknown>) => void;
// };

export interface Chainable<S, S1, F> {
  chainFrom: <F1>(task: Task<S, F>) => Task<S1, F | F1>;
};

interface Result<S, F> {
  success?: S;
  failure?: F;
}


export function isTask(value: unknown): value is Task<unknown, unknown> {
  return value instanceof Task;
}

export function isTaskResult(value: unknown): value is TaskOf<unknown> {
  return value instanceof TaskOf;
}

export class Task<S, F> {
  _state = "created" as "created" | "started" | "completed";
  _success: S | undefined;
  _failure: F | undefined;
  _context: object = {};

  _dependant: Task<unknown, unknown>;
  _start(queue: Queue) {}
  _step(queue: Queue) {}

  id: number;

  constructor() {
    if (this.constructor === Task) throw new Error("Don't call `new Task()`");
    this.id = uniqueId++;
  }

  static of<S>(value: S): Task<S, never> {
    return new TaskOf(value);
  }

  static rejected<F>(error: F): Task<never, F> {
    return new Rejected(error);
  }

  static empty(): Task<never, never> {
    return new Empty();
  }

  chain<S1, F1 extends F>(chainable: Chainable<S, S1, F>): Task<S1, F | F1> {
    return chainable.chainFrom(this);
  }
  chainFn<S1, F1 extends F>(fn: (task: Task<S1, F>, arg: S) => Task<S1, F | F1>): Task<S1, F | F1> {
    const newTask: Task<S1, F | F1> = new ChainFn(this, (task, arg) => fn(task, arg));
    return newTask
  }
  static concurrency<S, F>(tasks: Task<S, F>[]): Task<S[], F> {
    const newTask: Task<S[], F> = new ConcurrentTask(tasks);
    return newTask
  }

  wrap<T>(fn: (x: Task<S, F>) => T): T {
    return fn(this);
  }

  static waitFor<S, F>(event: Event<S, F>): Task<S, F> {
    return new WaitForEventTask(event)
  }

  _acceptContext(context: object) {
    this._context = Object.assign({}, this._context, context);
    return this;
  }

  _toString(): string { return "<abstract>"; }
  toString() { return `Task.${this._toString()}`; }
}

class TaskOf<S> extends Task<S, never> {
  _value: S;
  _state = 'completed' as const

  constructor(value: S) {
    super();
    this._value = value;
    this._success = value;
  }

  _toString() { return `of(${this.id}, ..)`; }
}

class Rejected<F> extends Task<never, F> {
  _error: F;

  constructor(error: F) {
    super();
    this._error = error;
  }

  _toString() { return "rejected(..)"; }
}

class Empty extends Task<never, never> {
  _toString() { return `empty()`; }
}

export class InitFn<SIn, SOut, F, F1 extends F> extends Task<SOut, F> implements Chainable<void, SOut, F> {
  _childTask: Task<SOut, F> | undefined;

  constructor(
    public fn: (task: Task<SOut, F>) => Task<SOut, F | F1>,
  ) {
    super();
  }

  chainFrom<F1 extends F>(task: Task<void, F>): Task<SOut, F | F1> {
    return new ChainFn(task, (task, arg: void) => 
      this.fn(task))
  }

  _start(queue: Queue) {
    this._state = "started";
    const newTask = this.fn(this);
    this._childTask = newTask;
    queue.enqueue(newTask);
  }
  _step(queue: Queue) {
    this._state = "completed";
    this._success = this._childTask!._success;
    this._failure = this._childTask!._failure;
  }

  _toString() { return `init(${this.id}, ..)`; }
}


export class ChainFn<SIn, SOut, F, F1> extends Task<SOut, F1 | F> {
  _task: Task<SIn, F> = undefined!;
  _childTask: Task<SOut, F | F1> | undefined;

  constructor(
    task: Task<SIn, F>,
    public fn: (task: Task<SOut, F1 | F>, arg: SIn) => Task<SOut, F | F1>
  ) {
    super();
    this._task = task;
    (task as any)._prevInChain = this
  }

  _start(queue: Queue) {
    this._state = "started";
    queue.enqueue(this._task);
  }
  _step(queue: Queue) {
    this._failure = this._task._failure;
    if (this._failure) {
      this._state = "completed";
    } else if (!this._childTask && this._task._success) {
      const newTask = this.fn(this, this._task._success);
      this._childTask = newTask
      queue.enqueue(newTask);
    } else if (this._childTask) {
      this._state = "completed";
      this._success = this._childTask._success;
      this._failure = this._childTask._failure;
    } else {
      console.log(this);
      throw new Error("Invalid state");
    }
  }

  _toString() {
    return `${this._task._toString()}.chain(${this.id}, ..)`;
  }
}
export class ConcurrentTask<SIn, F, F1> extends Task<SIn[], F1 | F> {
  _tasks: Task<SIn, F>[]

  constructor(
    tasks: Task<SIn, F>[],
  ) {
    super();
    this._tasks = tasks
    this._tasks.forEach((task) => (task as any)._prevInChain = this)
  }

  _start(queue: Queue) {
    this._state = "started";
    // Reverse order for debuggability
    for (let i = this._tasks.length-1; i >= 0; i--) {
      const task = this._tasks[i];
      queue.enqueue(task);
    }
  }
  _step(queue: Queue) {
    // this._state = "completed";
    let pending = 0;
    for (const task of this._tasks) {
      if (task._state === 'completed') {
        if (task!._failure !== undefined) {
          this._failure = task!._failure;
          this._state = 'completed'
          break
        }
      } else {
        pending ++;
      }
    }
    if (pending === 0 && this._state === 'started') {
      this._state = 'completed'
      this._success = this._tasks.map(x => x._success!)
    }
  }

  _toString() {
    return `concurrent(${this.id}, ${this._tasks.length}, ..)`;
  }
}

export class Event<S, F> {
  // public task: Yield<S, F> = undefined!
  _success: S | undefined
  _failure: F | undefined

  listeners: any[] = []

  success(s: S) {
    this._success = s;
    this.listeners.forEach(f => f(s));
    this.listeners.length = 0
  }
}

export class WaitForEventTask<S, F> extends Task<S, F> {

  constructor(
    public event: Event<S, F>
  ) {
    super();
  }

  _start(queue: Queue) {
    this._state = "started";
  }
  _step(queue: Queue) {
    
    if (this.event._success) {
      this._state = 'completed'
      this._success = this.event._success;
    }
    // TODO: handle failure
  }
  _waitForEvent(continuation: () => void) {
    this._state = 'started'

    this.event.listeners.push((success: S) => {
      continuation()
    });
  }


  _toString() {
    return `waitFor(${this.id})`;
  }
}

const onComplete = (queue: Queue, task: Task<unknown, unknown>) => {
  (task as any)._completeTick = queue.tick;
  queue.completed.push(task);
}

type UnknownTask = Task<unknown, unknown>;
export class Queue {
  tick: number = 0;
  currentTask: UnknownTask
  final: UnknownTask
  list: UnknownTask[] = []
  completed: UnknownTask[] = []
  allTasks: UnknownTask[] = []

  enqueue(task: Task<unknown, unknown>) {
    task._dependant = this.currentTask!;
    if (this.currentTask!) task._acceptContext(this.currentTask._context);
    if (task instanceof WaitForEventTask) {
      this.allTasks.push(task);
      task._waitForEvent(() => {
        // Different from enqueue because it doesn't update _dependant
        this.list.push(task)
      });
      return
    }
    console.log("pushed", task.toString());
    this.list.push(task);
    this.allTasks.push(task);
  }
}

export const stepQueue = (queue: Queue) => {
  if (queue.list.length === 0) return;
  queue.tick ++;
  const task = queue.list.pop()!;
  queue.currentTask = task;
  queue.final = task;
  console.log("Got", task.toString());

  if ((task as any)._startTick === undefined) {
    (task as any)._startTick = queue.tick;
  }

  if (task._state === "created") {
    task._start(queue);
  } else if (task._state === "started") {
    task._step(queue);
  }
  if ((task._state as unknown) === 'completed') {
    onComplete(queue, task)
    if (task._dependant) {
      // if (task._dependant instanceof ParallelTask) debugger
      if (task._dependant._state === 'completed') throw new Error("Already completed")
      queue.list.push(task._dependant);
    }
  }
};

type TaskDef<T, TParam, S, F> = { (param: TParam): Chainable<T, S, F> } &
  { of(arg: T, param: TParam): Task<S, F> & Chainable<void, S, F>; };

export const createTaskDef = <T, TParam, S, F>(
  computation: (context: object, arg: T, param: TParam) => Task<S, F>,
) => {
  const constructor = (param: TParam): Chainable<T, S, F> => {
    const chainFrom = <F1 extends F,>(task: Task<T, F>): Task<S, F | F1> => {
       const newTask : Task<S,F|F1> = new ChainFn(task, (task, arg) => computation(task._context, arg, param))
      //  newTask.toString = () => computation.name
       newTask.def = computation.name;
       return newTask;
    }
    return { chainFrom }
  };
  const of: TaskDef<T, TParam, S, F>["of"] = (arg: T, param: TParam) => {
    const task = new InitFn((task) => computation(task._context, arg, param))
    // task.toString = () => computation.name
    task.def = computation.name;
    return task;
  }
  // const _fn: TaskDef<T, TParam, S, F>["_fn"] = (task, arg, param) =>
  //   computation(task, arg, param);

  const taskDef: TaskDef<T, TParam, S, F> =
    Object.assign(constructor, { of }, );
  return taskDef;
};