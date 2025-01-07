import { capabilitiesLargerOrEqualTo, Capability, CapabilityRanking, compilerAssert, CompilerError, DiagnosticLocation, FunctionParameter, Type } from "../src/defs";
import { ControlFlowGraph, ControlFlowGraphGeneric, buildCFG, buildCFGFromRegions } from "../borrow/controlflow";
import { AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, AccessInstruction, ConditionalJumpInstruction, FunctionBlock, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, ReturnInstruction, StoreToAddressInstruction, GetFieldPointerInstruction, EndAccessInstruction, PhiInstruction, textColors, CommentInstruction, getInstructionResult, DeallocStackInstruction, CallExpressionNode, MarkInitializedInstruction, PointerOffsetInstruction, formatInstruction, ProjectBundleInstruction, YieldInstruction, BreakInstruction, GetGlobalAddress, BitCastInstruction, YieldGeneratorInstruction, JumpTableInstruction, ProjectAccessInstruction, PointerToAddressInstruction } from "../borrow/defs";
import { RegionWorklist } from "./initialization";
import { BlockRegion, InstructionId, IrDiagnostics, IrFunction, printIrFunction, RegionId } from "./region_codegen";

type BorrowedItem = {
  rootAddress: string;
  address: string;
  subObject: string;
  instructionId: InstructionId | null;
  capability: Capability;
  resultReg: string;
}

type LocalMap = Map<string, Set<string>>;
type MemoryMap = Map<string, BorrowSet>;

interface InterpreterState {
  locals: LocalMap; // Local variables/registers
  memory: MemoryMap // Memory addresses
}

type CFG = ControlFlowGraphGeneric<RegionId>

export class RegionExclusivityCheckingPass {
  state: InterpreterState

  cfg: CFG;
  blockStates: Map<RegionId, { input: InterpreterState, output: InterpreterState }> = new Map();
  function: IrFunction;
  freshAddressCounter = 0;
  addressTypes = new Map<string, Type>(); // Quick lookup for address types
  debugLog = true
  runs = 0
  instrId: InstructionId | null = null
  iterationIndex = 0
  diagnostics = new IrDiagnostics()
  globalsMap = new Map<string, string>()

  constructor(fn: IrFunction) {
    this.function = fn;
    // Build the CFG
    
  }

  checkedInterpret() {
    try {
      this.interpret()
    } catch (e) {
      if (e instanceof CompilerError) {
        if (!(e.info as any).location && this.instrId !== undefined) {
          Object.assign(e.info, { location: this.function.locations[this.instrId!] })
        }
      }
      this.printDebug()
      console.error(e)
      console.log("State:")
      // printLocals(this.state.locals)
      // printMemory(this.state.memory)
      throw e
    }
  }

  printDebug() {
    printIrFunction(this.function, this.diagnostics)
  }

  printLocals(locals: LocalMap) {
    this.diagnostics.instructionNote(this.instrId!, `  (${this.iterationIndex}) Locals: ${Array.from(locals.entries()).flatMap(([key, val]) => {
      if (val.size === 0) return `${key} -> ⊤`
      return `${key} -> ${Array.from(val).join(', ')}`
    }).join(' | ')}`)
  }
  printMemory(memory: MemoryMap) {
    this.diagnostics.instructionNote(this.instrId!, `  (${this.iterationIndex}) Memory: ${Array.from(memory.entries()).flatMap(([key, val]) => {
      if (val === undefined) return `${key} -> undefined`
      return `${key} -> ${borrowedItemsToString(val.borrows)}`
    }).join(' | ')}`)
  }

  createInitialState() {
    const state = createEmptyState();

    let i = 0
    for (const param of this.function.params) {
      const argIndex = i++;
      this.initializeFunctionParam(state, param, this.function.parameterRegisters[argIndex]);
    }
    if (this.function.returnParameter) {
      this.initializeFunctionParam(state, this.function.returnParameter, this.function.returnRegister)
    }

    for (const global of this.function.globalRegisters) {
      const newAddr = this.newAddress(global.type);
      this.globalsMap.set(global.register, newAddr)
      state.memory.set(newAddr, new BorrowSet());
    }
    return state;
    
  }

  interpret() {
    this.cfg = buildCFGFromRegions(this.function);


    console.log(textColors.green("\n\n#### Begin exclusivity check ####"))

    const entryState = this.createInitialState();

    const worklist = new RegionWorklist(this.cfg);

    const { regionId } = worklist.shift()!;
    this.executeRegion(regionId, entryState);
    worklist.visited.add(regionId);


    worklist.fixedPoint((regionId) => {
      this.runs += 1
      if (this.runs > 10000) {
        compilerAssert(false, "Infinite worklist loop", { runs: this.runs })
        return
      }
      const predecessors = this.cfg.predecessors.get(regionId) || [];
      const state = this.blockStates.get(regionId)!;
      const inputStates = predecessors.map(pred => this.blockStates.get(pred)!).filter(x => x);
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

      this.executeRegion(regionId, mergedInputState);
      worklist.addWork(regionId);
      worklist.visited.add(regionId);

      
    })

    console.log("All checked ok")
  }

  executeRegion(regionId: RegionId, inputState: InterpreterState) {
    const region = this.function.regions[regionId]
    compilerAssert(region instanceof BlockRegion, `Region is not a block region`); // CFG should only have block regions

    this.state = cloneState(inputState)
    if (this.debugLog) {
      // console.log(textColors.red(`\nExecuting block: ${regionId}`));
      // console.log("Input state for block:", regionId)

      this.printLocals(inputState.locals)
      this.printMemory(inputState.memory)
    }

    this.instrId = region.firstInstruction
    while (this.instrId) {
      const node = this.function.getInstructionNode(this.instrId)
      const instr = node?.instruction
      compilerAssert(instr, `Instruction found in block ${regionId}`, { regionId });
      // const instrId = new InstructionId(regionId, index);
      this.execute(this.instrId, instr);

      this.instrId = node.next
      if (this.iterationIndex++ > 10000) {
        compilerAssert(false, "Infinite instruction loop")
      }
    }

    if (this.debugLog) {
      // console.log("Computed state for block:", regionId)
      this.printLocals(this.state.locals)
      this.printMemory(this.state.memory)
    }

    this.blockStates.set(regionId, { input: inputState, output: cloneState(this.state) });
  }

  execute(instrId: InstructionId, instr: IRInstruction): void {
    if (this.debugLog) {
      // console.log(`Executing ${instr.irType}: ${instrId}`);
      // console.log(formatInstruction(instr));
    }
    if (instr instanceof AssignInstruction)               this.handleAssignInstruction(instr);
    else if (instr instanceof LoadConstantInstruction)    this.handleLoadConstantInstruction(instr);
    else if (instr instanceof AllocInstruction)           this.handleAllocInstruction(instr);
    else if (instr instanceof AccessInstruction)          this.access(instrId, instr);
    else if (instr instanceof LoadFromAddressInstruction) this.handleLoadFromAddressInstruction(instr);
    else if (instr instanceof GetFieldPointerInstruction) this.handleGetFieldPointerInstruction(instr);
    else if (instr instanceof PointerOffsetInstruction)   this.handlePointerOffsetInstruction(instr);
    else if (instr instanceof PointerToAddressInstruction)this.handlePointerToAddressInstruction(instr);
    else if (instr instanceof BinaryOperationInstruction) this.handleBinaryOperationInstruction(instr);
    else if (instr instanceof EndAccessInstruction)       this.endAccess(instrId, instr);
    else if (instr instanceof ProjectBundleInstruction)   this.handleProjectBundleInstruction(instrId, instr);
    else if (instr instanceof ProjectAccessInstruction)   this.handleProjectAccessInstruction(instrId, instr);
    else if (instr instanceof DeallocStackInstruction)    this.handleDeallocStackInstruction(instr);
    else if (instr instanceof YieldInstruction)           this.handleYieldInstruction(instr);
    else if (instr instanceof PhiInstruction)             this.handlePhiInstruction(instr);
    else if (instr instanceof GetGlobalAddress)           this.handleGetGlobalAddress(instr);
    else if (instr instanceof BitCastInstruction)         this.handleBitCastInstruction(instr);
    else if (instr instanceof YieldGeneratorInstruction)  this.handleYieldGeneratorInstruction(instr);
    else if (instr instanceof CallInstruction)            { }
    else if (instr instanceof MarkInitializedInstruction) { }
    else if (instr instanceof StoreToAddressInstruction)  { }
    else if (instr instanceof ReturnInstruction)          { }
    else if (instr instanceof BreakInstruction)           { }
    else if (instr instanceof JumpInstruction)            { }
    else if (instr instanceof JumpTableInstruction)       { }
    else if (instr instanceof ConditionalJumpInstruction) { }
    else if (instr instanceof CommentInstruction)         { }
    else compilerAssert(false, `Unknown instruction in exclusivity pass: ${instr.irType}`)
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
    this.state.memory.set(addr, new BorrowSet());
  }

  handleLoadFromAddressInstruction(instr: LoadFromAddressInstruction): void {
    const newAddress = this.addressFromRegister(instr.address)
    // const addresses = this.state.locals.get(instr.address);
    // this.state.locals.set(instr.dest, new Set(addresses));
    // const newAddress = this.newAddress(instr.type);
    this.state.locals.set(instr.dest, new Set([newAddress]));
    this.state.memory.set(newAddress, new BorrowSet());
  }

  handleGetFieldPointerInstruction(instr: GetFieldPointerInstruction): void {
    const addresses = this.state.locals.get(instr.address);
    compilerAssert(addresses, `Register ${instr.address} is not found`);
    compilerAssert(this.state.locals.get(instr.dest) === undefined, `Register ${instr.dest} is already initialized`);
    const fields = [...addresses].map(addr => `${addr}.${instr.field.index}`);
    this.state.locals.set(instr.dest, new Set(fields));
  }

  handlePointerOffsetInstruction(instr: PointerOffsetInstruction): void {
    if (this.debugLog) {
      // console.log("State before pointer offset")
      this.printMemory(this.state.memory)
      this.printLocals(this.state.locals)
    }
    const addresses = this.state.locals.get(instr.address);
    compilerAssert(addresses, `Register ${instr.address} is not found`);
    compilerAssert(this.state.locals.get(instr.dest) === undefined, `Register ${instr.dest} is already initialized`);
    // const fields = [...addresses].map(addr => `${addr}.pointer`);
    this.state.locals.set(instr.dest, new Set([]));
  }

  handlePointerToAddressInstruction(instr: PointerToAddressInstruction): void {
    const address = this.newAddress(instr.type);
    this.state.memory.set(address, new BorrowSet());
    this.state.locals.set(instr.dest, new Set([address]));
  }

  handleBitCastInstruction(instr: BitCastInstruction): void {
    // TODO:
    const addresses = this.state.locals.get(instr.source);
    compilerAssert(addresses, `Register ${instr.source} is not found`);
    this.state.locals.set(instr.dest, new Set([...addresses]));
  }

  handleBinaryOperationInstruction(instr: BinaryOperationInstruction): void {
    this.state.locals.set(instr.dest, new Set([]));
  }

  handlePhiInstruction(instr: PhiInstruction): void {
    // TODO: We should actually copy the state from the block
    // that we came from. Need a test case for this
    this.state.locals.set(instr.dest, new Set([]));
  }

  handleGetGlobalAddress(instr: GetGlobalAddress): void {
    const addr = this.globalsMap.get(instr.global)
    compilerAssert(addr, `Global ${instr.global} is not found`);
    this.state.locals.set(instr.dest, new Set([addr]));
    // const newAddr = this.newAddress(instr.type);
    // this.globalsMap.set(instr.global, newAddr)
    // this.state.locals.set(instr.dest, new Set([newAddr]));
    // this.state.memory.set(newAddr, new BorrowSet());
  }

  newAddress(type: Type): string {
    const addr = `a${this.freshAddressCounter++}`;
    this.addressTypes.set(addr, type)
    return addr;
  }

  addressFromRegister(reg: string): string {
    return `a_${reg}`;
  }

  initializeFunctionParam(state: InterpreterState, param: FunctionParameter, reg: string) {
    const addr = this.newAddress(param.type);
    state.locals.set(reg, new Set([addr]));

    // Exclusive borrow
    if (param.capability !== Capability.Let) {
      state.memory.set(addr, new BorrowSet())
      return
    }

    const bs = new BorrowSet()
    bs.insert(addr, param.capability, null)
    state.memory.set(addr, bs)
  }

  access(instrId: InstructionId, instr: AccessInstruction) {
    compilerAssert(instr.capabilities.length === 1, "Capability must have been reified by now")
    this.beginAccess(instr.dest, instr.source, instr.capabilities[0], instrId);
  }

  handleProjectBundleInstruction(instrId: InstructionId, instr: ProjectBundleInstruction) {
    compilerAssert(instr.capabilities.length === 1, "Capability must have been reified by now")
    this.beginAccess(instr.target, instr.source, instr.capabilities[0], instrId);
  }

  handleProjectAccessInstruction(instrId: InstructionId, instr: ProjectAccessInstruction) {
    compilerAssert(instr.capabilities.length === 1, "Capability must have been reified by now")
    this.beginAccess(instr.dest, instr.accessSource, instr.capabilities[0], instrId);
  }

  beginAccess(dest: string, source: string, capability: Capability, instrId: InstructionId) {
    const addrs = this.state.locals.get(source);
    compilerAssert(addrs, `No address found for ${source}`);
    const addrStr = Array.from(addrs).join(', ');
    // if (this.debugLog) console.log(`Accessing ${source} at ${addrStr} ${capability} to ${dest}`);

    const reborrowId = this.getReborrowSource(dest as InstructionId)

    // If there are no addresses in the set, it means that the value is a basic value
    // and access doesn't matter. So this part will be skipped
    for (const addr of addrs) {
      const ids = addr.split('.')
      const rootAddress = ids[0]
      let borrowSet = this.state.memory.get(rootAddress)?.clone()
      compilerAssert(borrowSet, `No memory state found for ${rootAddress}`);
      this.state.memory.set(rootAddress, borrowSet)

      if (borrowSet.borrows.length === 0) {
        borrowSet.insert(addr, capability, instrId, dest)
        // if (this.debugLog) printMemory(this.state.memory)
        continue
      }

      // if (this.debugLog) console.log("Existing borrows for address", instrId, addr)
      // if (this.debugLog) console.log("existingBorrows", borrowSet)

      const exclusiveBorrows = borrowSet.getExclusiveBorrows(addr, capability)

      if (exclusiveBorrows.length >= 1) {
        const allowedCapabilities = capabilitiesLargerOrEqualTo(capability);
        (() => {
          if (!reborrowId) return false
          // if (exclusiveBorrows[0].blockId !== reborrowId.blockId) return false
          if (exclusiveBorrows[0].instructionId !== reborrowId) return false
          if (!allowedCapabilities.includes(exclusiveBorrows[0].capability)) return false

          // console.log("Reborrowing")
          // console.log({ reborrowId, exclusiveBorrows })
          borrowSet.clear()
          exclusiveBorrows.length = 0
        })()
      }
      
      if (exclusiveBorrows.length > 0) {
        const str = capability === Capability.Let ? "already mutably borrowed" : "already borrowed"
        const location = this.function.locations[dest]
        const diagnosticLocations = exclusiveBorrows.map(b => new DiagnosticLocation(this.function.locations[b.instructionId!], `Borrowed here with ${b.capability} capability`))
        compilerAssert(false, `Cannot access with ${capability} (${str})`, { addr, dest, source, exclusiveBorrows, location, diagnosticLocations })
      }

      borrowSet.insert(addr, capability, instrId, dest)
      // console.log({ newBorrows: borrowSet.borrows })
    }

    this.state.locals.set(dest, addrs);
    
    if (this.debugLog) {
      // console.log("State after access")
      this.printMemory(this.state.memory)
      this.printLocals(this.state.locals)
    }
  }

  getReborrowSource(source: InstructionId) {
    // const sid = this.findInstructionIdByDest(source)!
    const s = this.function.getInstruction(source)
    const isAccess = s instanceof AccessInstruction ||
      s instanceof ProjectBundleInstruction ||
      s instanceof ProjectAccessInstruction
    compilerAssert(isAccess, "Expected access instruction")

    const getSource = (source2: InstructionId) => {
      const s2 = this.function.getInstruction(source2)
      if (s2 instanceof AccessInstruction) return source2
      if (s2 instanceof ProjectBundleInstruction) return source2
      if (s2 instanceof ProjectAccessInstruction) return source2
      if (s2 instanceof AssignInstruction) return getSource(s2.source as InstructionId)
      if (s2 instanceof GetFieldPointerInstruction) return getSource(s2.address as InstructionId)
      return null
    }

    const s2 = s instanceof ProjectAccessInstruction ? s.accessSource : s.source
    return getSource(s2 as InstructionId)
  }

  endAccess(instrId: InstructionId, instr: EndAccessInstruction) {
    const addrs = this.state.locals.get(instr.source);
    compilerAssert(addrs, `No address found for ${instr.source}`);
    compilerAssert(instr.capabilities.length === 1, "Capability must have been reified by now")
    const capability = instr.capabilities[0];
    const addrStr = Array.from(addrs).join(', ');
    // if (this.debugLog) console.log(`Ending access to ${instr.source} at ${addrStr} ${capability}`);


    const originalId = instr.source as InstructionId
    const originalInstr = this.function.getInstruction(originalId)
    const reborrowId = this.getReborrowSource(originalId)

    if (originalInstr instanceof ProjectAccessInstruction) {
      if (originalInstr.capabilities[0] === Capability.Sink) {
        // This is a bit of a workaround to handle moving out of a block AST
        // If it's a Sink capability then we don't know whether the value is moved
        // so we will hang on to the borrow indefinitely which prevents the user
        // from accessing the value again. Dealloc should still work although I
        // haven't confirmed this yet.
        // Check block_projection.rad and generateBlockExpression in codegen_ir.ts
        // We might want to formalize this better later in the actual IR representation
        // instead of putting this here - or maybe renaming the ProjectAccessInstruction
        // to better describe what it does
        return
      }
    }
    
    for (const addr of addrs) {
      const ids = addr.split('.')
      const rootAddress = ids[0]
      let borrowSet = this.state.memory.get(rootAddress)?.clone()
      compilerAssert(borrowSet, `No memory state found for ${addr}`);
      this.state.memory.set(rootAddress, borrowSet)

      let removeIndex = borrowSet.findIndex(capability, originalId)
      compilerAssert(removeIndex !== -1, "Could not find borrow to remove", { rootAddress, addr, capability, instrId, instr, borrowSet})

      // console.log({ removeIndex })

      borrowSet.removeIndex(removeIndex)
      // console.log("Removed borrow")
      // console.log("Remaining", borrowSet.borrows)

      if (reborrowId) {
        // const reborrow = this.cfg.blocks.find(b => b.label === reborrowId.blockId)!.instructions[reborrowId.instrId] as AccessInstruction | ProjectBundleInstruction
        const reborrow = this.function.getInstruction(reborrowId) as AccessInstruction | ProjectBundleInstruction
        if (reborrow.capabilities[0] === Capability.Let) {
          const foundIndex = borrowSet.findIndex(reborrow.capabilities[0], reborrowId)
          compilerAssert(foundIndex !== -1, "Expected existing borrow", { borrowSet, addr, capability: reborrow.capabilities[0], reborrowId })
        } else {
          borrowSet.insert(addr, reborrow.capabilities[0], reborrowId)
        }
      }
    }

    if (this.debugLog) this.printMemory(this.state.memory)
  }

  handleYieldInstruction(instr: YieldInstruction) {
    // Not sure yet
  }

  handleYieldGeneratorInstruction(instr: YieldGeneratorInstruction) {
    this.state.locals.set(instr.dest, new Set([]));
  }

  handleDeallocStackInstruction(instr: DeallocStackInstruction) {
    const locals = this.state.locals.get(instr.target);
    compilerAssert(locals, `Register ${instr.target} is not found`);
    compilerAssert(locals.size === 1, `Register ${instr.target} has multiple addresses`);
    this.state.memory.delete([...locals.values()][0]);
  }

}

class BorrowSet {
  borrows: BorrowedItem[] = [];
  constructor(borrows: BorrowedItem[] = []) {
    this.borrows = borrows;
  }

  clone() {
    return new BorrowSet([...this.borrows]);
  }

  insert(address: string, capability: Capability, instr: InstructionId | null, resultReg: string | null = null) {
    const ids = address.split('.')
    const rootAddress = ids[0]
    const subObject = ids.slice(1).join('.')
    const newBorrow: BorrowedItem = { 
      rootAddress,
      address, subObject, 
      instructionId: instr, capability, resultReg: resultReg! }
    this.borrows.push(newBorrow);
  }

  clear() {
    this.borrows.length = 0;
  }

  findIndex(capability: Capability, instr: InstructionId): number {
    return this.borrows.findIndex(b => {
      if (b.capability !== capability) return false
      // if (b.blockId === "" || b.instructionId === -1) return false // Parameter borrow
      if (b.instructionId === null) return false // Parameter borrow
      return b.instructionId === instr
    })
  }

  removeIndex(index: number) {
    this.borrows.splice(index, 1)
  }

  getExclusiveBorrows(address: string, capability: Capability): BorrowedItem[] {
    const ids = address.split('.')
    const subObject = ids.slice(1).join('.')

    const newBorrowIsLet = capability === Capability.Let
    const exclusiveBorrows: BorrowedItem[] = []
    for (const item of this.borrows) {
      const itemKey = item.subObject;
      const existingBorrowIsLet = item.capability === Capability.Let
      if (newBorrowIsLet && existingBorrowIsLet) continue

      if (itemKey.startsWith(subObject) || subObject.startsWith(itemKey)) {
        exclusiveBorrows.push(item)
      }
    }
    return exclusiveBorrows
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
    return `${key} -> ${borrowedItemsToString(val.borrows)}`
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
  const result = new Map<string, BorrowSet>();
  const allAddresses = new Set([...map1.keys(), ...map2.keys()]);
  for (const addr of allAddresses) {
    const val1 = map1.get(addr)?.borrows ?? [];
    const val2 = map2.get(addr)?.borrows ?? [];
    result.set(addr, new BorrowSet(meetBorrowedItems(val1, val2)));
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
    mapsEqual(state1.memory, state2.memory, borrowSetEqual)
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

function borrowSetEqual(bs1: BorrowSet, bs2: BorrowSet): boolean {
  return borrowedItemsEqual(bs1.borrows, bs2.borrows);
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
    // item1.blockId === item2.blockId &&
    item1.instructionId === item2.instructionId &&
    item1.capability === item2.capability
  );
}
