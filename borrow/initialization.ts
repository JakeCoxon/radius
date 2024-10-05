import { CodeGenerator } from "./codegen";
import { ControlFlowGraph, buildCFG } from "./controlflow";
import { AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, AccessInstruction, ConditionalJumpInstruction, FunctionBlock, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, ReturnInstruction, StoreToAddressInstruction, GetFieldPointerInstruction, compilerAssert, Type, StructType, Capability, EndAccessInstruction, PhiInstruction, MoveInstruction, VoidType, InstructionId, CommentInstruction, textColors, MarkInitializedInstruction, formatInstruction } from "./defs";
import { Worklist } from "./worklist";

type InitializationState = Top | Bottom | Sequence;

interface Top {
  kind: 'Top'; // Represents ⊤ (fully initialized)
}

interface Bottom {
  kind: 'Bottom'; // Represents ⊥ (fully uninitialized)
}

interface Sequence {
  kind: 'Sequence';
  elements: InitializationState[]; // Array of SD elements representing fields
}

const BOTTOM = { kind: 'Bottom' } as const;
const TOP = { kind: 'Top' } as const;

type LocalMap = Map<string, Set<string>>;
type MemoryMap = Map<string, InitializationState>;

interface InterpreterState {
  locals: LocalMap; // Local variables/registers
  memory: MemoryMap // Memory addresses
}

export class InitializationCheckingPass {
  state: InterpreterState

  cfg: ControlFlowGraph;
  blockStates: Map<string, { input: InterpreterState, output: InterpreterState }> = new Map();
  function: FunctionBlock;
  freshAddressCounter = 0;
  addressTypes = new Map<string, Type>(); // Quick lookup for address types
  debugLog = false
  currentBlock: BasicBlock | null = null
  currentInstr: IRInstruction | null = null
  instrIndex = 0
  codegen: CodeGenerator

  constructor(codegen: CodeGenerator, fn: FunctionBlock) {
    this.function = fn;
    this.codegen = codegen;
    // Build the CFG
    this.cfg = buildCFG(fn.blocks);
  }

  checkedInterpret() {
    try {
      this.interpret()
    } catch (e) {
      console.error(e)
      console.log("Current block", this.currentBlock?.label)
      console.log("Current instr", this.currentInstr)
      printLocals(this.state.locals)
      printMemory(this.state.memory)
      throw e
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
      console.log(textColors.red(`\nExecuting block: ${block.label}`));
      console.log("Input state for block:", block.label)

      printLocals(inputState.locals)
      printMemory(inputState.memory)
    }

    this.currentBlock = block;
    this.instrIndex = 0;
    while (this.instrIndex < block.instructions.length) {
      const instr = block.instructions[this.instrIndex];
      this.currentInstr = instr;
      const instrId = new InstructionId(block.label, this.instrIndex);
      this.execute(instrId, instr);
      this.instrIndex++;
    }

    if (this.debugLog) {
      console.log("Computed state for block:", block.label)
      printLocals(this.state.locals)
      printMemory(this.state.memory)
    }

    this.blockStates.set(block.label, { input: cloneState(inputState), output: cloneState(this.state) });
  }

  execute(instrId: InstructionId, instr: IRInstruction): void {
    if (this.debugLog) console.log(instrId.blockId, instrId.instrId+".", formatInstruction(instr))
    if (instr instanceof AssignInstruction) {
      this.ensureRegisterInitialized(instr.source);
      this.state.locals.set(instr.dest, this.state.locals.get(instr.source)!);
    } else if (instr instanceof LoadConstantInstruction) {
      this.state.locals.set(instr.dest, new Set([]));
    } else if (instr instanceof StoreToAddressInstruction) {
      this.ensureRegisterInitialized(instr.source);
      const addresses = this.state.locals.get(instr.address);
      compilerAssert(addresses, `Register ${instr.address} is not found`);
      for (const addr of addresses) {
        const ids = addr.split('.')
        const rootType = this.addressTypes.get(ids[0])
        compilerAssert(rootType, `Root type not found for address ${addr}`)
        this.updateMemoryAddressPathState(addr, rootType, TOP);
      }
      if (this.debugLog) {
        console.log("After StoreToAddressInstruction", instr.address, instr.source)
        printLocals(this.state.locals)
        printMemory(this.state.memory)
      }
    } else if (instr instanceof AllocInstruction) {
      const addr = this.newAddress(instr.type);
      this.state.locals.set(instr.dest, new Set([addr]));
      this.state.memory.set(addr, BOTTOM);
    } else if (instr instanceof AccessInstruction) {
      compilerAssert(instr.capabilities.length === 1, `Access instruction with multiple capabilities not supported`)
      if (instr.capabilities[0] === Capability.Inout) {
        this.ensureRegisterInitialized(instr.source);
      } else if (instr.capabilities[0] === Capability.Let) {
        this.ensureRegisterInitialized(instr.source);
      } else if (instr.capabilities[0] === Capability.Sink) {
        this.ensureRegisterInitialized(instr.source);
      } else if (instr.capabilities[0] === Capability.Set) {
        // this.ensureRegisterUninitialized(instr.source);
        if (!this.isDefinitelyUninitialized(instr.source)) {
          // TODO: For trivial types/builtin types we can just mark as uninitialized
          this.currentBlock!.instructions.splice(instrId.instrId, 0, new CommentInstruction(`Inserted uninitialize for ${instr.source}`))
          this.state.locals.get(instr.source)!.forEach(addr => {
            this.state.memory.set(addr, BOTTOM);
          })
          this.instrIndex++; // Skip the access instruction that would otherwise be visited since we inserted a new instruction
        }
      }
      this.state.locals.set(instr.dest, this.state.locals.get(instr.source)!);
    } else if (instr instanceof CallInstruction) {
      for (const arg of instr.args) {
        this.ensureRegisterInitialized(arg);
      }
      this.state.locals.get(instr.target)!.forEach(addr => {
        this.state.memory.set(addr, TOP);
      }) // Initialize the target
    } else if (instr instanceof BinaryOperationInstruction) {
      this.ensureRegisterInitialized(instr.left);
      this.ensureRegisterInitialized(instr.right);
      this.state.locals.set(instr.dest, new Set([]));
    } else if (instr instanceof LoadFromAddressInstruction) {
      this.ensureRegisterInitialized(instr.address);
      this.state.locals.set(instr.dest, this.state.locals.get(instr.address)!);
    } else if (instr instanceof ReturnInstruction) {
      if (instr.value) this.ensureRegisterInitialized(instr.value);
    } else if (instr instanceof GetFieldPointerInstruction) {
      const addresses = this.state.locals.get(instr.address);
      compilerAssert(addresses, `Register ${instr.address} is not found`);
      compilerAssert(this.state.locals.get(instr.dest) === undefined, `Register ${instr.dest} is already initialized`);
      const fields = [...addresses].map(addr => `${addr}.${instr.field}`)
      this.state.locals.set(instr.dest, new Set(fields));
      if (this.debugLog) {
        console.log("After GetFieldPointerInstruction", instr.address, instr.dest, fields)
        printLocals(this.state.locals)
        printMemory(this.state.memory)
      }
    } else if (instr instanceof JumpInstruction) {
    } else if (instr instanceof ConditionalJumpInstruction) {
      this.ensureRegisterInitialized(instr.condition);
    } else if (instr instanceof EndAccessInstruction) {
      // pass
    } else if (instr instanceof MoveInstruction) {
      if (this.debugLog) {
        console.log("MoveInstruction", instr.source, instr.target)
        printLocals(this.state.locals)
        printMemory(this.state.memory)
      }
      if (this.isDefinitelyInitialized(instr.target)) {
        this.emitMove(instrId, instr, Capability.Inout)
      } else if (this.isDefinitelyUninitialized(instr.target)) {
        this.emitMove(instrId, instr, Capability.Set)
      } else compilerAssert(false, `Target is not definitely initialized or uninitialized`)
    } else if (instr instanceof MarkInitializedInstruction) {
      const locals = this.state.locals.get(instr.target);
      compilerAssert(locals, `Register ${instr.target} is not found`);
      const newState = instr.initialized ? TOP : BOTTOM; 
      for (const addr of locals) {
        const ids = addr.split('.')
        const rootType = this.addressTypes.get(ids[0])
        this.updateMemoryAddressPathState(addr, rootType!, newState)
      }
      if (this.debugLog) {
        console.log("After MarkInitializedInstruction", instr.target, instr.initialized)
        printLocals(this.state.locals)
        printMemory(this.state.memory)
      }
    } else if (instr instanceof PhiInstruction) {
      // TODO: We should actually copy the state from the block
      // that we came from. Need a test case for this
      this.state.locals.set(instr.dest, new Set([]));
    } else if (instr instanceof CommentInstruction) {
      // pass
    } else {
      console.error(`Unknown instruction in initialization pass: ${instr.irType}`);
    }
  }

  isDefinitelyInitialized(register: string): boolean {
    const locals = this.state.locals.get(register);
    compilerAssert(locals, `Register ${register} is not found`);
    return Array.from(locals).every(addr => {
      return isStatePathInitialized(this.state.memory, addr);
    })
  }

  isDefinitelyUninitialized(register: string): boolean {
    const locals = this.state.locals.get(register);
    compilerAssert(locals, `Register ${register} is not found`);
    return Array.from(locals).every(addr => {
      return !isStatePathInitialized(this.state.memory, addr);
    })
  }
  ensureRegisterInitialized(register: string) {
    const isInitialized = this.isDefinitelyInitialized(register);
    compilerAssert(isInitialized, `Register ${register} is not definitely initialized`);
  }

  ensureRegisterUninitialized(register: string) {
    const isUninitialized = this.isDefinitelyUninitialized(register);
    compilerAssert(isUninitialized, `Register ${register} is not definitely uninitialized`);
  }

  ensureAddressInitialized(addr: string) {
  }

  newAddress(type: Type): string {
    const addr = `a${this.freshAddressCounter++}`;
    this.addressTypes.set(addr, type)
    return addr;
  }

  updateMemoryAddressPathState(addr: string, rootType: Type, newState: InitializationState) {
    const ids = addr.split('.')
    let current = this.state.memory.get(ids[0])
    compilerAssert(current, `Address ${addr} is not initialized`)
    
    const statePath = createStatePathState(ids.slice(1), rootType)
    const newSd = meetInitializationStatePath(current, statePath, newState)
    this.state.memory.set(ids[0], newSd)
  }

  initializeFunctionParam(state: InterpreterState, param: string, type: Type) {
    const addr = this.newAddress(type);
    state.locals.set(param, new Set([addr]));
    state.memory.set(addr, TOP);
  }

  emitMove(instrId: InstructionId, instr: MoveInstruction, capability: Capability) {
    const block = this.currentBlock!;
    const sourceAccessReg = this.codegen.newRegister();
    const targetAccessReg = this.codegen.newRegister();
    const instrs = [
      new CommentInstruction(`Replaced move with ${capability} to ${instr.target} from ${instr.source}`),
      new AccessInstruction(sourceAccessReg, instr.source, [Capability.Sink]),
      new AccessInstruction(targetAccessReg, instr.target, [capability]),
      new MarkInitializedInstruction(targetAccessReg, true),
      new MarkInitializedInstruction(sourceAccessReg, false),
      // new EndAccessInstruction(instrId, instr.target)
    ];
    block.instructions.splice(instrId.instrId, 1, ...instrs);
    this.instrIndex -- // Revert the index to the start of the inserted instructions
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
  return false
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

function initializationStateToString(sd: InitializationState): string {
  if (sd.kind === 'Top') { return '⊤'; }
  if (sd.kind === 'Bottom') { return '⊥'; }
  if (sd.kind === 'Sequence') {
    return `[${sd.elements.map(initializationStateToString).join(', ')}]`;
  }
  return '???';
}

const createStatePathState = (ids: string[], currentType: Type): { id: number, numFields: number }[] => {
  if (ids.length === 0) return []
  const [firstAddr, ...restAddrs] = ids
  const index = parseInt(firstAddr, 10)
  compilerAssert(currentType instanceof StructType, `Current type is not a struct type`, { currentType })
  compilerAssert(!isNaN(index), `Index is not a number`)
  compilerAssert(index >= 0, `Index is negative`)
  compilerAssert(index < currentType.fields.length, `Index is out of bounds`)
  const nextType = currentType.fields[index].type
  return [{ id: index, numFields: currentType.fields.length }, ...createStatePathState(restAddrs, nextType)]
}

function meetInitializationStatePath(a: InitializationState, statePath: { id: number, numFields: number }[], newState: InitializationState): InitializationState {
  if (statePath.length === 0) return newState;

  const length = statePath[0].numFields;
  const elements: InitializationState[] = [];
  let allTop = true;
  let allBottom = true;
  for (let i = 0; i < length; i++) {
    const elemA = a.kind === 'Sequence' ? a.elements[i] : a
    let newElem = elemA

    if (i === statePath[0].id) {
      newElem = meetInitializationStatePath(elemA, statePath.slice(1), newState);
    } 
    elements.push(newElem);
    
    allTop = allTop && newElem.kind === 'Top';
    allBottom = allBottom && newElem.kind === 'Bottom';
  }
  
  if (allTop) { return TOP; }
  else if (allBottom) { return BOTTOM; }
  return { kind: 'Sequence', elements };
}

function meetInitializationState(a: InitializationState, b: InitializationState): InitializationState {
  if (a.kind === 'Bottom' || b.kind === 'Bottom') { return BOTTOM; }
  if (a.kind === 'Top') { return b; }
  if (b.kind === 'Top') { return a; }
  if (a.kind === 'Sequence' && b.kind === 'Sequence') {
    const length = Math.max(a.elements.length, b.elements.length);
    const elements: InitializationState[] = [];
    let allTop = true;
    for (let i = 0; i < length; i++) {
      const elemA = a.elements[i] || BOTTOM;
      const elemB = b.elements[i] || BOTTOM;
      elements.push(meetInitializationState(elemA, elemB));
      allTop = allTop && elements[i].kind === 'Top';
    }
    if (allTop) { return TOP; }
    return { kind: 'Sequence', elements };
  }
  // Incompatible types, default to Bottom
  compilerAssert(false, `Incompatible types in meetSD: ${a.kind} and ${b.kind}`);
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
  const result = new Map<string, InitializationState>();
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

function initializationStateEqual(sd1: InitializationState, sd2: InitializationState): boolean {
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
