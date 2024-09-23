import { ControlFlowGraph, buildCFG } from "./controlflow";
import { AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, CheckInitializedInstruction, ConditionalJumpInstruction, FunctionBlock, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, ReturnInstruction, StoreToAddressInstruction, GetFieldPointerInstruction, compilerAssert, Type, StructType } from "./defs";

type SD = Top | Bottom | Sequence;

interface Top {
  kind: 'Top'; // Represents ⊤ (fully initialized)
}

interface Bottom {
  kind: 'Bottom'; // Represents ⊥ (fully uninitialized)
}

interface Sequence {
  kind: 'Sequence';
  elements: SD[]; // Array of SD elements representing fields
}

type LocalMap = Map<string, Set<string>>;
type MemoryMap = Map<string, SD>;

interface InterpreterState {
  locals: LocalMap; // Local variables/registers
  memory: MemoryMap // Memory addresses
}

export class AbstractInterpreterIR {
  state: InterpreterState

  cfg: ControlFlowGraph;
  blockStates: Map<BasicBlock, InterpreterState> = new Map();
  worklist: { block: BasicBlock; inputState: InterpreterState }[] = [];
  function: FunctionBlock;
  freshAddressCounter = 0;
  addressTypes = new Map<string, Type>(); // Quick lookup for address types

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

    for (const block of this.cfg.blocks) {
      this.blockStates.set(block, undefined!);
    }

    const entryState = createEmptyState();
    this.worklist.push({ block: this.cfg.entry, inputState: entryState });

    for (const param of this.function.params) {
      this.initializeFunctionParam(entryState, param.name, param.type);
    }

    // Process the worklist
    while (this.worklist.length > 0) {
      const { block, inputState } = this.worklist.shift()!;
      const existingState = this.blockStates.get(block);

      // Merge the new input state with the existing state
      const mergedInputState = existingState
        ? mergeStates(existingState, inputState)
        : inputState;

      // If the merged state is the same as the existing state, skip processing
      if (existingState && statesEqual(existingState, mergedInputState)) {
        continue;
      }

      this.blockStates.set(block, mergedInputState);

      const outputState = this.executeBlock(block, mergedInputState);

      // Add successor blocks to the worklist
      const successors = this.cfg.successors.get(block) || [];
      for (const successor of successors) {
        this.worklist.push({ block: successor, inputState: outputState });
      }
    }

    console.log("All checked ok")
  }

  executeBlock(block: BasicBlock, inputState: InterpreterState): InterpreterState {
    // Set the state to the input state
    this.state = cloneState(inputState);

    let index = 0;
    while (index < block.instructions.length) {
      const instr = block.instructions[index];
      this.execute(instr);
      index++;
    }

    // Return the state after executing the block
    return cloneState(this.state);
  }

  execute(instr: IRInstruction): void {
    if (instr instanceof AssignInstruction) {
      this.ensureRegisterInitialized(instr.source);
      this.state.locals.set(instr.dest, this.state.locals.get(instr.source)!);
    } else if (instr instanceof LoadConstantInstruction) {
      this.state.locals.set(instr.dest, new Set([]));
    } else if (instr instanceof StoreToAddressInstruction) {
      this.ensureRegisterInitialized(instr.source);
      const addresses = this.state.locals.get(instr.address);
      if (!addresses) {
        throw new Error(`Address ${instr.address} is not initialized`);
      }
      for (const addr of addresses) {
        const ids = addr.split('.')
        const rootType = this.addressTypes.get(ids[0])
        compilerAssert(rootType, `Root type not found for address ${addr}`)
        this.initializeMemoryAddress(addr, rootType);
      }
    } else if (instr instanceof AllocInstruction) {
      const addr = this.newAddress(instr.type);
      this.state.locals.set(instr.dest, new Set([addr]));
      this.state.memory.set(addr, { kind: 'Bottom' });
    } else if (instr instanceof CheckInitializedInstruction) {
      this.ensureRegisterInitialized(instr.value);
    } else if (instr instanceof CallInstruction) {
      this.state.locals.set(instr.dest, new Set([]));
    } else if (instr instanceof BinaryOperationInstruction) {
      this.state.locals.set(instr.dest, new Set([]));
    } else if (instr instanceof LoadFromAddressInstruction) {
      this.ensureRegisterInitialized(instr.address);
      this.state.locals.set(instr.dest, this.state.locals.get(instr.address)!);
    } else if (instr instanceof ReturnInstruction) {
      if (instr.value) this.ensureRegisterInitialized(instr.value);
    } else if (instr instanceof GetFieldPointerInstruction) {
      const addresses = this.state.locals.get(instr.address);
      if (!addresses) {
        throw new Error(`Register ${instr.address} is not found`);
      }
      compilerAssert(this.state.locals.get(instr.dest) === undefined, `Register ${instr.dest} is already initialized`);
      const fields = [...addresses].map(addr => `${addr}.${instr.field}`)
      this.state.locals.set(instr.dest, new Set(fields));

    } else {
      console.error(`Unknown instruction in interp: ${instr.irType}`);
    }
    // No action needed for JumpInstruction and ConditionalJumpInstruction here
  }

  ensureRegisterInitialized(register: string) {
    const locals = this.state.locals.get(register);
    if (!locals) {
      throw new Error(`Register ${register} is not found`);
    }
    const isInitialized = Array.from(locals).every(addr => {
      return isStatePathInitialized(this.state.memory, addr);
    })
    if (!isInitialized) {
      throw new Error(`Register ${register} is not initialized`);
    }
  }

  ensureAddressInitialized(addr: string) {
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
    const sd = addressesToSD(ids.slice(1), rootType)
    const newSd = meetSDUpper(current, sd)
    this.state.memory.set(ids[0], newSd)
  }

  initializeFunctionParam(state: InterpreterState, param: string, type: Type) {
    const addr = this.newAddress(type);
    state.locals.set(param, new Set([addr]));
    state.memory.set(addr, { kind: 'Top' });
  }

}

const createEmptyState = (): InterpreterState => {
  return {
    locals: new Map(),
    memory: new Map()
  };
}

function isStatePathInitialized(memory: MemoryMap, addr: string): boolean {
  const ids = addr.split('.')
  let current = memory.get(ids[0])
  
  for (let i = 1; i < ids.length; i++) {
    if (!current || current.kind === 'Bottom') { return false; }
    if (current.kind === 'Top') { return true; }
    current = current.elements[parseInt(ids[i])];
  }

  if (!current || current.kind === 'Bottom') { return false; }
  if (current.kind === 'Top') { return true; }
  // return current.elements.every(e => e.kind === 'Top');
  return false
}

function sdToString(sd: SD): string {
  if (sd.kind === 'Top') { return '⊤'; }
  if (sd.kind === 'Bottom') { return '⊥'; }
  if (sd.kind === 'Sequence') {
    return `[${sd.elements.map(sdToString).join(', ')}]`;
  }
  return '???';
}

function meetSDUpper(a: SD, b: SD): SD {
  if (a.kind === 'Top' || b.kind === 'Top') { return { kind: 'Top' }; }
  if (a.kind === 'Bottom') { return b; }
  if (b.kind === 'Bottom') { return a; }
  if (a.kind === 'Sequence' && b.kind === 'Sequence') {
    const length = Math.max(a.elements.length, b.elements.length);
    const elements: SD[] = [];
    let allTop = true;
    for (let i = 0; i < length; i++) {
      const elemA = a.elements[i] || { kind: 'Bottom' };
      const elemB = b.elements[i] || { kind: 'Bottom' };
      elements.push(meetSDUpper(elemA, elemB));
      allTop = allTop && elements[i].kind === 'Top';
    }
    if (allTop) { return { kind: 'Top' }; }
    return { kind: 'Sequence', elements };
  }
  // Incompatible types, default to Bottom
  compilerAssert(false, `Incompatible types in meetSDUpper: ${a.kind} and ${b.kind}`);
}

function meetSD(a: SD, b: SD): SD {
  if (a.kind === 'Bottom' || b.kind === 'Bottom') { return { kind: 'Bottom' }; }
  if (a.kind === 'Top') { return b; }
  if (b.kind === 'Top') { return a; }
  if (a.kind === 'Sequence' && b.kind === 'Sequence') {
    const length = Math.max(a.elements.length, b.elements.length);
    const elements: SD[] = [];
    let allTop = true;
    for (let i = 0; i < length; i++) {
      const elemA = a.elements[i] || { kind: 'Bottom' };
      const elemB = b.elements[i] || { kind: 'Bottom' };
      elements.push(meetSD(elemA, elemB));
      allTop = allTop && elements[i].kind === 'Top';
    }
    if (allTop) { return { kind: 'Top' }; }
    return { kind: 'Sequence', elements };
  }
  // Incompatible types, default to Bottom
  compilerAssert(false, `Incompatible types in meetSD: ${a.kind} and ${b.kind}`);
}

function addressesToSD(addresses: string[], currentType: Type): SD {
  if (addresses.length === 0) { return { kind: 'Top' }; }
  const [firstAddr, ...restAddrs] = addresses;
  const index = parseInt(firstAddr, 10);

  compilerAssert(currentType instanceof StructType, `Current type is not a struct type`)
  compilerAssert(!isNaN(index), `Index is not a number`)
  compilerAssert(index >= 0, `Index is negative`)
  compilerAssert(index < currentType.fields.length, `Index is out of bounds`)

  const nextType = currentType.fields[index].type
  const nestedSD = addressesToSD(restAddrs, nextType);

  const elements: SD[] = new Array(currentType.fields.length).fill({ kind: 'Bottom' });
  elements[index] = nestedSD;

  return { kind: 'Sequence', elements };
}

function meetLocals(a: Set<string> | undefined, b: Set<string> | undefined): Set<string> {
  const set = new Set<string>();
  if (a) { a.forEach(v => set.add(v)); }
  if (b) { b.forEach(v => set.add(v)); }
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
    result.set(key, meetLocals(val1, val2));
  }
  return result;
}

function mergeMemoryMaps(
  map1: MemoryMap,
  map2: MemoryMap
): MemoryMap {
  const result = new Map<string, SD>();
  const allAddresses = new Set([...map1.keys(), ...map2.keys()]);
  for (const addr of allAddresses) {
    const val1 = map1.get(addr) || { kind: 'Bottom' };
    const val2 = map2.get(addr) || { kind: 'Bottom' };
    result.set(addr, meetSD(val1, val2));
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
    mapsEqual(state1.memory, state2.memory, sdEqual)
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

function sdEqual(sd1: SD, sd2: SD): boolean {
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
      if (!sdEqual(sd1.elements[i], sd2.elements[i])) {
        return false;
      }
    }
    return true;
  }
  return false;
}