import { Capability, Type } from "../src/defs";
import { ControlFlowGraph, buildCFG } from "./controlflow";
import { AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, AccessInstruction, ConditionalJumpInstruction, FunctionBlock, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, ReturnInstruction, StoreToAddressInstruction, GetFieldPointerInstruction, compilerAssert, EndAccessInstruction, PhiInstruction, FunctionParameter, textColors, InstructionId, CommentInstruction, getInstructionResult } from "./defs";
import { Worklist } from "./worklist";

type BorrowedItem = {
  rootAddress: string;
  address: string;
  subObject: string;
  blockId: string
  instructionId: number;
  capability: Capability;
  resultReg: string;
}

type LocalMap = Map<string, Set<string>>;
type MemoryMap = Map<string, BorrowedItem[]>;

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
  runs = 0

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
      throw e
    }
  }

  interpret() {

    const entryState = createEmptyState();

    console.log(textColors.green("\n\n#### Begin exclusivity check ####"))

    let i = 0
    for (const param of this.function.params) {
      const argIndex = i++;
      this.initializeFunctionParam(entryState, param.binding, this.function.parameterRegisters[argIndex]);
    }

    const worklist = new Worklist(this.cfg);

    const { block } = worklist.shift()!;
    this.executeBlock(block, entryState);
    worklist.visited.add(block);


    worklist.fixedPoint((block) => {
      this.runs += 1
      if (this.runs > 100) {
        compilerAssert(false, "Infinite loop")
        return
      }
      const predecessors = this.cfg.predecessors.get(block) || [];
      const state = this.blockStates.get(block.label)!;
      const inputStates = predecessors.map(pred => this.blockStates.get(pred.label)!).filter(x => x);
      const mergedInputState = inputStates.slice(1).reduce((acc, predState) => {
        return mergeStates(acc, predState.output);
      }, inputStates[0].output);
      
      if (this.debugLog) {
        // console.log("\n## Block", block.label, "\n")
        // console.log("immediate dominator", this.cfg.getImmediateDominator(block)?.label)
        // console.log("num predecessors", predecessors.length)
        // console.log("num input states", inputStates.length)
        // console.log("predecessors", predecessors.map(pred => pred.label))
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

    let index = 0;
    while (index < block.instructions.length) {
      const instr = block.instructions[index];
      const instrId = new InstructionId(block.label, index);
      this.execute(instrId, instr);
      index++;
    }

    if (this.debugLog) {
      console.log("Computed state for block:", block.label)
      printLocals(this.state.locals)
      printMemory(this.state.memory)
    }

    this.blockStates.set(block.label, { input: inputState, output: cloneState(this.state) });
  }

  execute(instrId: InstructionId, instr: IRInstruction): void {
    if (this.debugLog) console.log(`Executing ${instr.irType} ${instrId.blockId}:${instrId.instrId}`);
    if (instr instanceof AssignInstruction)               this.handleAssignInstruction(instr);
    else if (instr instanceof LoadConstantInstruction)    this.handleLoadConstantInstruction(instr);
    else if (instr instanceof AllocInstruction)           this.handleAllocInstruction(instr);
    else if (instr instanceof AccessInstruction)          this.access(instrId, instr);
    else if (instr instanceof LoadFromAddressInstruction) this.handleLoadFromAddressInstruction(instr);
    else if (instr instanceof GetFieldPointerInstruction) this.handleGetFieldPointerInstruction(instr);
    else if (instr instanceof BinaryOperationInstruction) this.handleBinaryOperationInstruction(instr);
    else if (instr instanceof JumpInstruction)            { }
    else if (instr instanceof ConditionalJumpInstruction) { }
    else if (instr instanceof EndAccessInstruction)       this.endAccess(instrId, instr);
    else if (instr instanceof PhiInstruction)             this.handlePhiInstruction(instr);
    else if (instr instanceof CommentInstruction)         { }
    else console.error(`Unknown instruction in exclusivity pass: ${instr.irType}`)
  }

  handleAssignInstruction(instr: AssignInstruction): void {
    this.state.locals.set(instr.dest, this.state.locals.get(instr.source)!);
  }

  handleLoadConstantInstruction(instr: LoadConstantInstruction): void {
    this.state.locals.set(instr.dest, new Set([]));
  }

  handleAllocInstruction(instr: AllocInstruction): void {
    const addr = this.newAddress(instr.type);
    this.state.locals.set(instr.dest, new Set([addr]));
    this.state.memory.set(addr, []);
  }

  handleLoadFromAddressInstruction(instr: LoadFromAddressInstruction): void {
    this.state.locals.set(instr.dest, new Set([]));
  }

  handleGetFieldPointerInstruction(instr: GetFieldPointerInstruction): void {
    const addresses = this.state.locals.get(instr.address);
    compilerAssert(addresses, `Register ${instr.address} is not found`);
    compilerAssert(this.state.locals.get(instr.dest) === undefined, `Register ${instr.dest} is already initialized`);
    const fields = [...addresses].map(addr => `${addr}.${instr.field}`);
    this.state.locals.set(instr.dest, new Set(fields));
  }

  handleBinaryOperationInstruction(instr: BinaryOperationInstruction): void {
    this.state.locals.set(instr.dest, new Set([]));
  }

  handlePhiInstruction(instr: PhiInstruction): void {
    // TODO: We should actually copy the state from the block
    // that we came from. Need a test case for this
    this.state.locals.set(instr.dest, new Set([]));
  }

  newAddress(type: Type): string {
    const addr = `a${this.freshAddressCounter++}`;
    this.addressTypes.set(addr, type)
    return addr;
  }

  initializeFunctionParam(state: InterpreterState, param: FunctionParameter, reg: string) {
    const addr = this.newAddress(param.type);
    state.locals.set(reg, new Set([addr]));
    state.memory.set(addr, []); // TODO: Handle immutable/mutable
  }

  access(instrId: InstructionId, instr: AccessInstruction) {
    const addrs = this.state.locals.get(instr.source);
    compilerAssert(addrs, `No address found for ${instr.source}`);
    compilerAssert(instr.capabilities.length === 1, "Capability must have been reified by now")
    const capability = instr.capabilities[0];
    const addrStr = Array.from(addrs).join(', ');
    if (this.debugLog) console.log(`Accessing ${instr.source} at ${addrStr} ${capability} to ${instr.dest}`);

    for (const addr of addrs) {
      const ids = addr.split('.')
      const rootAddress = ids[0]
      let existingBorrows = this.state.memory.get(rootAddress);
      compilerAssert(existingBorrows, `No memory state found for ${rootAddress}`);
      const newBorrows = [...existingBorrows];
      this.state.memory.set(rootAddress, newBorrows);
      const subObject = ids.slice(1).join('.')
      const newBorrow: BorrowedItem = { 
        rootAddress,
        address: addr, subObject, blockId: instrId.blockId, 
        instructionId: instrId.instrId, capability, resultReg: instr.dest }
      if (existingBorrows.length === 0) {
        newBorrows.push(newBorrow)
        if (this.debugLog) {
          console.log("Add access", newBorrow)
          printMemory(this.state.memory)
        }
        continue
      }

      if (this.debugLog) console.log("Existing borrows for address", instrId, addr)
      if (this.debugLog) console.log("existingBorrows", existingBorrows)

      const newBorrowIsLet = capability === Capability.Let

      const exclusiveBorrows: BorrowedItem[] = []
      for (const item of existingBorrows) {
        const itemKey = item.subObject;
        const existingBorrowIsLet = item.capability === Capability.Let
        if (newBorrowIsLet && existingBorrowIsLet) continue

        if (itemKey.startsWith(subObject) || subObject.startsWith(itemKey)) {
          exclusiveBorrows.push(item)
        }
      }
      if (exclusiveBorrows.length > 0) {
        compilerAssert(false, "Cannot access (already mutably borrowed)", { exclusiveBorrows })
      }
      
      newBorrows.push(newBorrow)

      if (this.debugLog) console.log("newBorrows", newBorrows)
      // compilerAssert(false, "Not implemented")
    }
    this.state.locals.set(instr.dest, addrs);
    if (this.debugLog) {
      console.log("State after access")
      printMemory(this.state.memory)
    }
  }

  endAccess(instrId: InstructionId, instr: EndAccessInstruction) {
    const addrs = this.state.locals.get(instr.source);
    compilerAssert(addrs, `No address found for ${instr.source}`);
    compilerAssert(instr.capabilities.length === 1, "Capability must have been reified by now")
    const capability = instr.capabilities[0];
    const addrStr = Array.from(addrs).join(', ');
    if (this.debugLog) console.log(`Ending access to ${instr.source} at ${addrStr} ${capability}`);

    for (const addr of addrs) {
      const ids = addr.split('.')
      const rootAddress = ids[0]
      let existingBorrows = this.state.memory.get(rootAddress);
      compilerAssert(existingBorrows, `No memory state found for ${addr}`);
      const removeIndex = existingBorrows.findIndex(b => {
        if (b.address !== addr) return false
        if (b.capability !== capability) return false
        const block = this.cfg.blocks.find(bl => bl.label === b.blockId)!;
        const result = getInstructionResult(block.instructions[b.instructionId])
        if (result !== instr.source) return false
        return true
      })
      compilerAssert(removeIndex !== -1, "Could not find borrow to remove", {
        rootAddress, addr, capability, instrId, instr, existingBorrows
      })
      // Make sure to make a copy of the array
      const newBorrows = existingBorrows.filter((_, i) => i !== removeIndex)
      this.state.memory.set(rootAddress, newBorrows)
    }
    if (this.debugLog) {
      console.log("State after end access")
      printMemory(this.state.memory)
    }
    // this.state.locals.delete(instr.source);
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
    if (val === undefined) return `${key} -> undefined`
    return `${key} -> ${borrowedItemsToString(val)}`
  }).join(' | '))
}

function borrowedItemsToString(sd: BorrowedItem[]): string {
  if (sd === undefined) compilerAssert(false, "Undefined state")
  if (sd.length === 0) return 'Unique'
  const r = sd.map(b => `${b.capability}(${b.subObject})`).join(', ')
  return `<${r}>`
}

function meetBorrowedItems(a: BorrowedItem[], b: BorrowedItem[]): BorrowedItem[] {
  let mergedArray: BorrowedItem[] = [];
  let i = 0, j = 0;

  while (i < a.length || j < b.length) {
    const item1 = a[i];
    const item2 = b[j];

    if (!item1) { mergedArray.push(item2); j++; continue; }
    if (!item2) { mergedArray.push(item1); i++; continue; }

    if (borrowedItemEqual(item1, item2)) {
      mergedArray.push(item1); i++; j++;
    } else if (item1.subObject < item2.subObject) {
      mergedArray.push(item1); i++;
    } else {
      mergedArray.push(item2); j++;
    }
  }

  return mergedArray;
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
  const result = new Map<string, BorrowedItem[]>();
  const allAddresses = new Set([...map1.keys(), ...map2.keys()]);
  for (const addr of allAddresses) {
    const val1 = map1.get(addr) ?? [];
    const val2 = map2.get(addr) ?? [];
    result.set(addr, meetBorrowedItems(val1, val2));
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
    mapsEqual(state1.memory, state2.memory, borrowedItemsEqual)
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

function borrowedItemsEqual(sd1: BorrowedItem[], sd2: BorrowedItem[]): boolean {
  if (sd1.length !== sd2.length) return false;
  for (const item of sd1) {
    if (!sd2.find(i => borrowedItemEqual(i, item))) {
      return false;
    }
  }
  return true;
}

function borrowedItemEqual(item1: BorrowedItem, item2: BorrowedItem): boolean {
  return (
    item1.address === item2.address &&
    item1.subObject === item2.subObject &&
    item1.blockId === item2.blockId &&
    item1.instructionId === item2.instructionId &&
    item1.capability === item2.capability
  );
}
