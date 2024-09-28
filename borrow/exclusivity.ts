import { ControlFlowGraph, buildCFG } from "./controlflow";
import { AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, AccessInstruction, ConditionalJumpInstruction, FunctionBlock, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, ReturnInstruction, StoreToAddressInstruction, GetFieldPointerInstruction, compilerAssert, Type, StructType, Capability } from "./defs";
import { Worklist } from "./worklist";

type ExclusivityState = Unique | Borrowed | Aggregate;

interface Unique {
  kind: 'Unique';
}

interface Borrowed {
  kind: 'Borrowed';
  instructioons: Set<string>;
}

interface Aggregate {
  kind: 'Aggregate';
  elements: ExclusivityState[];
}

const UNIQUE = { kind: 'Unique' } as const;

type LocalMap = Map<string, Set<string>>;
type MemoryMap = Map<string, ExclusivityState>;

interface InterpreterState {
  locals: LocalMap; // Local variables/registers
  memory: MemoryMap // Memory addresses
}

export class ExclusivityCheckingPass {
  state: InterpreterState

  cfg: ControlFlowGraph;
  blockStates: Map<string, { input: InterpreterState, output: InterpreterState }> = new Map();
  function: FunctionBlock;
  freshAddressCounter = 0;
  addressTypes = new Map<string, Type>(); // Quick lookup for address types
  debugLog = false

  constructor(fn: FunctionBlock) {
    this.function = fn;
    // Build the CFG
    this.cfg = buildCFG(fn.blocks);
  }

  checkedInterpret() {
    try {
      this.interpret()
    } catch (e) {
      console.error(e)
      console.log("State:")
      console.log(this.state)
    }
  }

  interpret() {

    const entryState = createEmptyState();

    for (const param of this.function.params) {
      this.initializeFunctionParam(entryState, param.name, param.type);
    }

    const worklist = new Worklist(this.cfg);

    const { block } = worklist.shift()!;
    this.executeBlock(block, entryState);
    worklist.visited.add(block);

    worklist.fixedPoint((block) => {
      const predecessors = this.cfg.predecessors.get(block) || [];
      const state = this.blockStates.get(block.label)!;
      const inputStates = predecessors.map(pred => this.blockStates.get(pred.label)!).filter(x => x);
      const mergedInputState = inputStates.slice(1).reduce((acc, predState) => {
        return mergeStates(acc, predState.output);
      }, inputStates[0].output);
      
      if (this.debugLog) {
        console.log("\n## Block", block.label, "\n")
        console.log("immediate dominator", this.cfg.getImmediateDominator(block)?.label)
        console.log("num predecessors", predecessors.length)
        console.log("num input states", inputStates.length)
        console.log("predecessors", predecessors.map(pred => pred.label))
      }

      const allInputStates = inputStates.length === predecessors.length

      if (state && allInputStates && statesEqual(state.input, mergedInputState)) return

      this.executeBlock(block, mergedInputState);
      worklist.addWork(block);
      worklist.visited.add(block);
    })

    console.log("All checked ok")
  }

  executeBlock(block: BasicBlock, inputState: InterpreterState) {

    this.state = cloneState(inputState)
    if (this.debugLog) {
      console.log("\nExecuting block:", block.label);
      console.log("Input state for block:", block.label)

      printLocals(inputState.locals)
      printMemory(inputState.memory)
    }

    let index = 0;
    while (index < block.instructions.length) {
      const instr = block.instructions[index];
      this.execute(instr);
      index++;
    }

    if (this.debugLog) {
      console.log("Computed state for block:", block.label)
      printLocals(this.state.locals)
      printMemory(this.state.memory)
    }

    this.blockStates.set(block.label, { input: inputState, output: cloneState(this.state) });
  }

  execute(instr: IRInstruction): void {
    if (instr instanceof AssignInstruction) {
    } else if (instr instanceof LoadConstantInstruction) {
    } else if (instr instanceof StoreToAddressInstruction) {
    } else if (instr instanceof AllocInstruction) {
    } else if (instr instanceof AccessInstruction) {
    } else if (instr instanceof CallInstruction) {
    } else if (instr instanceof BinaryOperationInstruction) {
    } else if (instr instanceof LoadFromAddressInstruction) {
    } else if (instr instanceof ReturnInstruction) {
    } else if (instr instanceof GetFieldPointerInstruction) {
    } else if (instr instanceof JumpInstruction) {
    } else if (instr instanceof ConditionalJumpInstruction) {
    } else {
      console.error(`Unknown instruction in interp: ${instr.irType}`);
    }
  }

  newAddress(type: Type): string {
    const addr = `a${this.freshAddressCounter++}`;
    this.addressTypes.set(addr, type)
    return addr;
  }

  initializeMemoryAddress(addr: string, rootType: Type) {
    const ids = addr.split('.')
    let current = this.state.memory.get(ids[0])
    compilerAssert(current, `Address ${addr} is not initialized`)
    const sd = addressPathToInitializationState(ids.slice(1), rootType)
    const newSd = meetInitializationStateUpper(current, sd)
    this.state.memory.set(ids[0], newSd)
  }

  initializeFunctionParam(state: InterpreterState, param: string, type: Type) {
    const addr = this.newAddress(type);
    state.locals.set(param, new Set([addr]));
    state.memory.set(addr, TOP);
  }

}

const createEmptyState = (): InterpreterState => {
  return {
    locals: new Map(),
    memory: new Map()
  };
}

function printLocals(locals: LocalMap) {
  console.log("  Locals:", Array.from(locals.entries()).flatMap(([key, val]) => {
    if (val.size === 0) return `${key} -> ⊤`
    return `${key} -> ${Array.from(val).join(', ')}`
  }).join(' | '))
}
function printMemory(memory: MemoryMap) {
  console.log("  Memory:", Array.from(memory.entries()).flatMap(([key, val]) => {
    return `${key} -> ${initializationStateToString(val)}`
  }).join(' | '))
}

function initializationStateToString(sd: ExclusivityState): string {
  if (sd.kind === 'Unique') return 'Unique'
  if (sd.kind === 'Borrowed') return 'Borrowed'
  if (sd.kind === 'Aggregate') {
    return sd.elements.map(initializationStateToString).join(' | ')
  }
  return '???';
}

function meetInitializationStateUpper(a: ExclusivityState, b: ExclusivityState): ExclusivityState {
}

function meetInitializationState(a: ExclusivityState, b: ExclusivityState): ExclusivityState {
}

function meetLocals(a: Set<string>, b: Set<string>): Set<string> {
  const set = new Set<string>();
  a.forEach(v => set.add(v));
  b.forEach(v => set.add(v));
  return set
}

function mergeLocalMaps(
  map1: LocalMap,
  map2: LocalMap
): LocalMap {
  const result = new Map<string, Set<string>>();
  const allKeys = new Set([...map1.keys(), ...map2.keys()]);
  for (const key of allKeys) {
    const val1 = map1.get(key)
    const val2 = map2.get(key)
    if (val1 === undefined || val2 === undefined) continue
    result.set(key, meetLocals(val1, val2));
  }
  return result;
}

function mergeMemoryMaps(
  map1: MemoryMap,
  map2: MemoryMap
): MemoryMap {
  const result = new Map<string, ExclusivityState>();
  const allAddresses = new Set([...map1.keys(), ...map2.keys()]);
  for (const addr of allAddresses) {
    const val1 = map1.get(addr) || BOTTOM;
    const val2 = map2.get(addr) || BOTTOM;
    result.set(addr, meetInitializationState(val1, val2));
  }
  return result;
}

function mergeStates(
  state1: InterpreterState,
  state2: InterpreterState
): InterpreterState {
  return {
    locals: mergeLocalMaps(state1.locals, state2.locals),
    memory: mergeMemoryMaps(state1.memory, state2.memory)
  };
}

function cloneState(state: InterpreterState): InterpreterState {
  return {
    locals: new Map(state.locals),
    memory: new Map(state.memory)
  };
}

function mapsEqual<K, V>(
  map1: Map<K, V>,
  map2: Map<K, V>,
  valueEqual: (v1: V, v2: V) => boolean
): boolean {
  if (map1.size !== map2.size) {
    return false;
  }
  for (const [key, val1] of map1) {
    const val2 = map2.get(key);
    if (!val2 || !valueEqual(val1, val2)) {
      return false;
    }
  }
  return true;
}

function statesEqual(state1: InterpreterState, state2: InterpreterState): boolean {
  return (
    mapsEqual(state1.locals, state2.locals, localsEqual) &&
    mapsEqual(state1.memory, state2.memory, initializationStateEqual)
  );
}

function localsEqual(locals1: Set<string>, locals2: Set<string>): boolean {
  if (locals1.size !== locals2.size) {
    return false;
  }
  for (const local of locals1) {
    if (!locals2.has(local)) {
      return false;
    }
  }
  return true;
}

function initializationStateEqual(sd1: ExclusivityState, sd2: ExclusivityState): boolean {
  if (sd1.kind !== sd2.kind) {
    return false;
  }
  if (sd1.kind === 'Top' || sd1.kind === 'Bottom') {
    return true;
  }
  if (sd1.kind === 'Sequence') {
    if (sd2.kind !== 'Sequence') { return false; }
    if (sd1.elements.length !== sd2.elements.length) {
      return false;
    }
    for (let i = 0; i < sd1.elements.length; i++) {
      if (!initializationStateEqual(sd1.elements[i], sd2.elements[i])) {
        return false;
      }
    }
    return true;
  }
  return false;
}
