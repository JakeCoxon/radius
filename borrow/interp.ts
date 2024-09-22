import { ControlFlowGraph, buildCFG } from "./controlflow";
import { AllocInstruction, AssignInstruction, BasicBlock, BinaryOperationInstruction, CallInstruction, CheckInitializedInstruction, ConditionalJumpInstruction, FunctionBlock, IRInstruction, JumpInstruction, LoadConstantInstruction, LoadFromAddressInstruction, LoadFieldInstruction, ReturnInstruction, StoreFieldInstruction, StoreToAddressInstruction } from "./defs";

export type InitState = 'uninitialized' | 'initialized' | AggregateState;

export interface InterpreterState {
  registers: { [name: string]: InitState };
  memory: { [address: string]: InitState };
}

export class AggregateState {
  constructor (public fields: {[field: string]: InitState}) {}
}

const initializeField = (state: InitState | undefined, field: string): AggregateState => {
  if (state === undefined) {
    return new AggregateState({ [field]: 'initialized' });
  }
  if (state === 'uninitialized') {
    return new AggregateState({ [field]: 'initialized' });
  }
  if (state === 'initialized') {
    return new AggregateState({ [field]: 'initialized' });
  }
  return new AggregateState({ ...state.fields, [field]: 'initialized' });
}

// Interpreter
export class AbstractInterpreterIR {
  state: InterpreterState = {
    registers: {},
    memory: {},
  };

  cfg: ControlFlowGraph;
  blockStates: Map<BasicBlock, InterpreterState> = new Map();
  worklist: { block: BasicBlock; inputState: InterpreterState }[] = [];
  function: FunctionBlock;

  constructor(fn: FunctionBlock) {
    this.function = fn;
    // Build the CFG
    this.cfg = buildCFG(fn.blocks);
  }

  interpret() {

    // Initialize block states
    for (const block of this.cfg.blocks) {
      this.blockStates.set(block, undefined);
    }

    // Add entry block to worklist with an empty state
    const entryState = this.createEmptyState();
    this.worklist.push({ block: this.cfg.entry, inputState: entryState });

    for (const param of this.function.params) {
      entryState.registers[param.name] = 'initialized';
      entryState.memory[`addr_${param.name}`] = 'initialized';
    }

    // Process the worklist
    while (this.worklist.length > 0) {
      const { block, inputState } = this.worklist.shift()!;
      const existingState = this.blockStates.get(block);

      // Merge the new input state with the existing state
      const mergedInputState = existingState
        ? this.mergeStates(existingState, inputState)
        : inputState;

      // If the merged state is the same as the existing state, skip processing
      if (existingState && this.statesEqual(existingState, mergedInputState)) {
        continue;
      }

      // Update the block's input state
      this.blockStates.set(block, mergedInputState);

      // Execute the block
      const outputState = this.executeBlock(block, mergedInputState);

      // Add successor blocks to the worklist
      const successors = this.cfg.successors.get(block) || [];
      for (const successor of successors) {
        this.worklist.push({ block: successor, inputState: outputState });
      }
    }
  }

  executeBlock(block: BasicBlock, inputState: InterpreterState): InterpreterState {
    // Set the state to the input state
    this.state = this.cloneState(inputState);

    let index = 0;
    while (index < block.instructions.length) {
      const instr = block.instructions[index];
      this.execute(instr);
      index++;
    }

    // Return the state after executing the block
    return this.cloneState(this.state);
  }

  execute(instr: IRInstruction): void {
    console.log('instr', instr.irType)
    if (instr instanceof AssignInstruction) {
      this.ensureRegisterInitialized(instr.source);
      this.state.registers[instr.dest] = 'initialized';
      const sourceAddr = `addr_${instr.source}`;
      console.log('sourceAddr', sourceAddr)
      if (sourceAddr in this.state.memory) {
        this.ensureAddressInitialized(sourceAddr);
        const destAddr = `addr_${instr.dest}`;
        console.log('destAddr', destAddr)
        this.state.memory[destAddr] = 'initialized';
      }
    } else if (instr instanceof LoadConstantInstruction) {
      this.state.registers[instr.dest] = 'initialized';
    } else if (instr instanceof StoreToAddressInstruction) {
      const addr = `addr_${instr.address}`;
      if (!(addr in this.state.memory)) {
        console.error(`Memory at address '${addr}' is not allocated.`);
        console.log('this.state.memory', this.state.memory)
        console.log('this.state.registers', this.state.registers)
        throw new Error();
      }
      this.state.memory[addr] = 'initialized';
    } else if (instr instanceof AllocInstruction) {
      // register is initialized but memory is not
      this.state.registers[instr.dest] = 'initialized';
      const address = `addr_${instr.dest}`;
      this.state.memory[address] = 'uninitialized'
    } else if (instr instanceof CheckInitializedInstruction) {
      const addr = `addr_${instr.value}`;
      this.ensureAddressInitialized(addr);
    } else if (instr instanceof CallInstruction) {
      console.log('call', instr.args)
      console.log('this.state.registers', this.state.registers)
      console.log('this.state.memory', this.state.memory)
      for (const reg of instr.args) {
        console.log('reg', reg) 
        this.ensureRegisterInitialized(reg);
      }
      this.state.registers[instr.dest] = 'initialized'
    } else if (instr instanceof BinaryOperationInstruction) {
      this.ensureRegisterInitialized(instr.left);
      this.ensureRegisterInitialized(instr.right);
      this.state.registers[instr.dest] = 'initialized'
    } else if (instr instanceof StoreFieldInstruction) {
      this.ensureRegisterInitialized(instr.address);
      const addr = `addr_${instr.address}`;
      if (!(addr in this.state.memory)) {
        console.error(`Memory at address '${addr}' is not allocated.`);
        console.log('this.state.memory', this.state.memory)
        console.log('this.state.registers', this.state.registers)
        throw new Error();
      }
      this.state.memory[addr] = initializeField(this.state.memory[addr], instr.field)
    } else if (instr instanceof LoadFromAddressInstruction) {
      const addr = `addr_${instr.address}`;
      this.ensureAddressInitialized(addr);
      this.state.registers[instr.dest] = 'initialized'
    } else if (instr instanceof ReturnInstruction) {
      if (instr.value) this.ensureRegisterInitialized(instr.value);
    } else if (instr instanceof LoadFieldInstruction) {
      this.ensureRegisterInitialized(instr.address);
      const loadAddr = `addr_${instr.address}`;
      const loadFieldState = this.state.memory[loadAddr];
      if (loadFieldState === undefined) {
        console.error(`Field '${instr.field}' at address '${loadAddr}' is not initialized.`);
        throw new Error();
      } else if (loadFieldState === 'uninitialized') {
        console.error(
          `Field '${instr.field}' at address '${loadAddr}' is used before initialization.`
        );
        throw new Error();
      } else if (loadFieldState instanceof AggregateState) {
        const fieldState = loadFieldState.fields[instr.field];
        if (fieldState === undefined) {
          console.error(
            `Field '${instr.field}' at address '${loadAddr}' is not initialized.`
          );
          throw new Error();
        } else if (fieldState === 'uninitialized') {
          console.error(
            `Field '${instr.field}' at address '${loadAddr}' is used before initialization.`
          );
          throw new Error();
        }
        console.warn(
          `Field '${instr.field}' at address '${loadAddr}' may be used before initialization.`
        );
        throw new Error();
      }
      this.state.registers[instr.dest] = 'initialized'
      // this.state.registers[instr.dest] = fieldInitState;
    } else {
      console.error(`Unknown instruction in interp: ${instr.irType}`);
    }
    // No action needed for JumpInstruction and ConditionalJumpInstruction here
  }

  ensureRegisterInitialized(register: string) {
    const regState = this.state.registers[register];
    if (regState === undefined || regState === 'uninitialized') {
      console.error(`Register '${register}' is used before initialization.`);
      throw new Error();
    } else if (regState instanceof AggregateState) {
      console.warn(`Register '${register}' may be used before initialization.`);
      throw new Error();
    }
  }

  ensureAddressInitialized(addr: string) {
    const state = this.state.memory[addr];
    if (state === undefined || state === 'uninitialized') {
      console.error(`Memory at address '${addr}' is not initialized.`);
      console.error('this.state.memory', this.state.memory)
      throw new Error();
    } else if (state instanceof AggregateState) {
      console.error(`Memory at address '${addr}' is partially initialized.`);
      console.error('this.state.memory', this.state.memory)
      throw new Error();
    }
  }

  cloneState(state: InterpreterState): InterpreterState {
    return {
      registers: { ...state.registers },
      memory: JSON.parse(JSON.stringify(state.memory)),
    };
  }

  mergeStates(state1: InterpreterState, state2: InterpreterState): InterpreterState {
    const mergedRegisters: { [name: string]: InitState } = {};
    const allRegisters = new Set([
      ...Object.keys(state1.registers),
      ...Object.keys(state2.registers),
    ]);

    for (const reg of allRegisters) {
      const stateReg1 = state1.registers[reg];
      const stateReg2 = state2.registers[reg];
      mergedRegisters[reg] = this.mergeInitStates(stateReg1, stateReg2);
    }

    const mergedMemory: { [address: string]: InitState } = {};
    const allAddresses = new Set([
      ...Object.keys(state1.memory),
      ...Object.keys(state2.memory),
    ]);

    for (const addr of allAddresses) {
      const mem1 = state1.memory[addr] || 'uninitialized'
      const mem2 = state2.memory[addr] || 'uninitialized'
      mergedMemory[addr] = this.mergeInitStates(mem1, mem2);
    }

    return {
      registers: mergedRegisters,
      memory: mergedMemory,
    };
  }

  statesEqual(state1: InterpreterState, state2: InterpreterState): boolean {
    return JSON.stringify(state1) === JSON.stringify(state2);
  }

  mergeInitStates(state1: InitState, state2: InitState): InitState {
    if (state1 === state2) {
      return state1;
    }
    if (state1 === undefined) return state2;
    if (state2 === undefined) return state1;
    if (state1 === 'uninitialized') return 'uninitialized';
    if (state2 === 'uninitialized') return 'uninitialized';
    if (state1 === 'initialized') return state2;
    if (state2 === 'initialized') return state1;
    return this.mergeAggregateStates(state1 as AggregateState, state2 as AggregateState);
  }

  mergeAggregateStates(
    agg1: AggregateState,
    agg2: AggregateState
  ): AggregateState {
    const mergedAgg: AggregateState = new AggregateState({});
    const allFields = new Set([...Object.keys(agg1), ...Object.keys(agg2)]);

    for (const field of allFields) {
      const stateField1 = agg1.fields[field];
      const stateField2 = agg2.fields[field];
      mergedAgg.fields[field] = this.mergeInitStates(stateField1, stateField2);
    }

    return mergedAgg;
  }

  createEmptyState(): InterpreterState {
    return { registers: {}, memory: {} };
  }
}
