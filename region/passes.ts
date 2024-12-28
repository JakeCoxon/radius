import { CodeGenerator, FunctionCodeGenerator } from "../borrow/codegen_ir";
import { FunctionBlock, Module } from "../borrow/defs";
import { Ast, Binding, CompilerError, GlobalCompilerState, textColors } from "../src/defs";
import { RegionExclusivityCheckingPass } from "./exclusivity";
import { RegionInitializationCheckingPass } from "./initialization";
import { InlineRegionProjectBundlesPass } from "./inlining";
import { CloseRegionAccessPass } from "./liveness";
import { IrFunction, printIrFunction, SequenceId } from "./region_codegen";
import { RegionReifyAccessPass } from "./reifyaccess";


export function runCodegenPasses(globalCompiler: GlobalCompilerState) {

  const codeGenerator = new CodeGenerator(globalCompiler);
  const mod = new Module();
  mod.functionMap = globalCompiler.compiledFunctions;
  codeGenerator.functions = globalCompiler.compiledFunctions;

  const compiledRegionIr = new Map<Binding, IrFunction>();

  globalCompiler.globalVars.forEach((gv) => {
    gv.register = codeGenerator.newGlobalId();
  });

  globalCompiler.compiledIr = new Map();
  globalCompiler.compiledFunctions.forEach((func) => {
    if (!func.body) return;
    const fnGenerator = codeGenerator.functionGenerator(func);
    let fn;
    try {
      fn = fnGenerator.generateFunction(func.binding, func.parameters, func.returnType, func.body);
      runMandatoryPasses(fnGenerator, mod, fn, func.body);
    } catch (ex) {
      if (ex instanceof CompilerError) {
        Object.assign(ex.info, { functionName: func.binding.name });
      }
      throw ex;
    }

    globalCompiler.compiledIr.set(func.binding, fn);
    codeGenerator.irFunctions.set(func.binding, fnGenerator.regionCodegen.irFunction);
    compiledRegionIr.set(func.binding, fnGenerator.regionCodegen.irFunction);
  });


  const inline = new InlineRegionProjectBundlesPass(globalCompiler, codeGenerator, compiledRegionIr);
  inline.inlineRegionProjectBundlesPass();
  globalCompiler.compiledRegionIr = compiledRegionIr;
}


const runMandatoryPasses = (fnGenerator: FunctionCodeGenerator, mod: Module, fn: FunctionBlock, body: Ast) => {
  const DebugLog = false

  console.log(textColors.yellow(`\n// ${fn.name} ///////////////////////////////////////////////////////////\n`));
  fn.params.forEach((p, i) => {
    console.log(textColors.yellow(`// ${p.binding.name}: ${p.capability} ${p.type.shortName} - ref ${p.reference} - ${fn.parameterRegisters[i]}`));
  })

  // console.dir(body, { depth: 10 });


  // Filter out blocks that are not reachable from the entry block
  // const cfgFirst = buildCFG(fn.blocks)
  // fn.blocks = cfgFirst.blocks.filter(b => cfgFirst.predecessors.get(b)!.length > 0 || b === cfgFirst.entry)
  
  // if (DebugLog) printIR(fn.blocks);

  // const cfg = buildCFG(fn.blocks)
  // printCFG(cfg)
  // printDominators(cfg)

  // const reify = new ReifyAccessPass(cfg, fn);
  // reify.debugLog = DebugLog;
  // reify.reifyAccesses();

  const irFunction = fnGenerator.regionCodegen.irFunction

  let initPass = new RegionInitializationCheckingPass(fnGenerator.regionCodegen, irFunction)
  let closeAccessPass = new CloseRegionAccessPass(fnGenerator.regionCodegen)
  let exclPass = new RegionExclusivityCheckingPass(irFunction)
  
  try {

    irFunction.sequences.forEach((seq, seqId) => {
      if (seq.firstChildRegion === null) {
        const regionId = fnGenerator.regionCodegen.insertNewBlockRegion()
        fnGenerator.regionCodegen.insertSequenceChild(seqId as SequenceId, regionId)
      }
    })

    printIrFunction(irFunction)

    const reify2 = new RegionReifyAccessPass(irFunction)
    reify2.debugLog = true;
    reify2.reifyAccesses();

    initPass.checkedInterpret()
    closeAccessPass.insertRegionCloseAccesses()
    exclPass.checkedInterpret()

  } catch (ex) {
    throw ex
  } finally {

    // closeAccessPass.printDebug()
    // initPass.printDebug()

  }

  // if (DebugLog) printIR(fn.blocks);

  // const interpreter = new InitializationCheckingPass(fnGenerator, mod, fn);
  // interpreter.debugLog = DebugLog;
  // interpreter.checkedInterpret();

  // console.log("Initialized")
  // if (DebugLog) printIR(fn.blocks);

  // console.log("")
  // insertCloseAccesses(cfg, fn.blocks, DebugLog)

  // console.log("Closed access")
  // if (DebugLog) printIR(fn.blocks);

  // const interpreter2 = new ExclusivityCheckingPass(fn)
  // interpreter2.debugLog = DebugLog;
  // interpreter2.checkedInterpret();
  // console.log("")

  // console.log(``);
  // if (DebugLog) printIR(fn.blocks);
  console.log(`\n/// finished ${fn.name} ///\n`);
}
