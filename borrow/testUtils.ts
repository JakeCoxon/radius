import { externalBuiltinBindings } from "../src/compiler_sugar";
import { CompiledFunction, IntType, BoolType, VoidType, RawPointerType, TypeField, TypeInfo, CompiledClass, SourceLocation, ConcreteClassType, FunctionParameter, SetFieldAst, PrimitiveType, VoidAst, LetAst, OperatorAst, ConstructorAst, FunctionDefinition, ReturnAst, IfAst, AndAst, WhileAst, isType, Binding, Type, BindingAst, StatementsAst, FieldAst, CallAst, Ast, NumberAst, SetAst, Capability } from "../src/defs";
import { createParameter, generateConstructor, generateMoveFunction } from "./codegen_ast";
import { CodeGenerator } from "./codegen_ir";
import { ASTNode, ProgramNode, BlockStatementNode, FunctionDeclarationNode, LetConstNode, VariableDeclarationNode, LiteralNode, compilerAssert, ExpressionStatementNode, BinaryExpressionNode, AssignmentNode, IdentifierNode, CreateStructNode, MemberExpressionNode, ReturnNode, CallExpressionNode, PrintNode, IfStatementNode, AndNode, WhileStatementNode } from "./defs";

class Scope {
  constants: Record<string, any> = {};
  variables: Record<string, Binding> = {};
  functions: Record<string, CompiledFunction> = {};
}

export class BasicCompiler {

  scopeStack: Scope[] = [new Scope()];
  rootScope: Scope = this.scopeStack[0];
  scope: Scope = this.rootScope;

  allFunctions: Map<Binding, CompiledFunction> = new Map();

  constructor(public codegen: CodeGenerator) {
    this.defineConstant('int', IntType);
    this.defineConstant('bool', BoolType);
    this.defineConstant('void', VoidType);
    this.defineConstant('ptr', RawPointerType);
    this.defineType('Point', [
      { name: 'x', type: 'int' },
      { name: 'y', type: 'int' },
    ]);
    this.defineType('Line', [
      { name: 'p1', type: 'Point' },
      { name: 'p2', type: 'Point' },
    ]);
  }

  defineConstant(name: string, value: any) {
    this.scope.constants[name] = value;
  }

  getConstant(name: string) {
    let i = this.scopeStack.length - 1;
    while (i >= 0) {
      if (this.scopeStack[i].constants[name]) { return this.scopeStack[i].constants[name]; }
      i--;
    }
  }

  getVariable(name: string) {
    let i = this.scopeStack.length - 1;
    while (i >= 0) {
      if (this.scopeStack[i].variables[name]) { return this.scopeStack[i].variables[name]; }
      i--;
    }
  }

  getFunction(name: string) {
    let i = this.scopeStack.length - 1;
    while (i >= 0) {
      if (this.scopeStack[i].functions[name]) { return this.scopeStack[i].functions[name]; }
      i--;
    }
  }

  defineType(name: string, fields: { name: string; type: string; }[]) {
    const typeFields: TypeField[] = [];
    const typeInfo: TypeInfo = { sizeof: 0, fields: typeFields, metaobject: {}, isReferenceType: false };
    const binding = new Binding(name, VoidType);
    const compiledClass = new CompiledClass(SourceLocation.anon, name, binding, null!, null!, null!, typeFields, [], 0);
    let structType = new ConcreteClassType(compiledClass, typeInfo);

    fields.forEach((f, i) => {
      const type = this.getType(f.type);
      typeFields[i] = new TypeField(SourceLocation.anon, f.name, structType, i, type);
    });

    this.defineConstant(name, structType);
    const constructor = generateConstructor(name, structType);
    typeInfo.metaobject.constructorBinding = constructor.binding;

    const moveInit = generateMoveFunction(structType, `moveInit${name}`, Capability.Set, Capability.Sink);
    const moveAssign = generateMoveFunction(structType, `moveAssign${name}`, Capability.Inout, Capability.Sink);
    const copyConstructor = generateMoveFunction(structType, `copy${name}`, Capability.Set, Capability.Let);

    Object.assign(typeInfo.metaobject, { 
      moveInitBinding: moveInit.binding,
      moveAssignBinding: moveAssign.binding,
      copyConstructorBinding: copyConstructor.binding,
    })

    this.allFunctions.set(constructor.binding, constructor)
    this.allFunctions.set(moveInit.binding, moveInit)
    this.allFunctions.set(moveAssign.binding, moveAssign)
    this.allFunctions.set(copyConstructor.binding, copyConstructor)

    return structType;
  }

  compile(node: ASTNode): Ast {

    if (node instanceof ProgramNode) {
      const body = new BlockStatementNode(node.body);
      return this.compile(new FunctionDeclarationNode('main', [], 'void', body));
    }

    if (node instanceof BlockStatementNode) {
      const statements = node.body.map(x => this.compile(x));
      return new StatementsAst(VoidType, SourceLocation.anon, statements);
    }

    if (node instanceof LetConstNode) {
      this.defineConstant(node.name, node.value);
      return new VoidAst(VoidType, SourceLocation.anon);
    }

    if (node instanceof VariableDeclarationNode) {
      const type = this.getType(node.type);
      const binding = new Binding(node.name, type);
      this.scope.variables[node.name] = binding;
      const value = node.initializer ? this.compile(node.initializer) : null;
      return new LetAst(VoidType, SourceLocation.anon, binding, value, node.mutable);
    }

    if (node instanceof LiteralNode) {
      if (typeof node.value === 'number') {
        return new NumberAst(IntType, SourceLocation.anon, node.value);
      }
      compilerAssert(false, 'Not implemented LiteralNode', { ast: node });
    }

    if (node instanceof ExpressionStatementNode) {
      const expression = this.compile(node.expression);
      return expression;
    }

    if (node instanceof BinaryExpressionNode) {
      const left = this.compile(node.left);
      const right = this.compile(node.right);
      compilerAssert(left.type === right.type, 'Type mismatch', { left, right });
      return new OperatorAst(left.type, SourceLocation.anon, node.operator, [left, right]);
    }

    if (node instanceof AssignmentNode) {
      const left = this.compile(node.left);
      const right = this.compile(node.right);
      compilerAssert(left.type === right.type, 'Type mismatch', { left, right, leftType: left.type, rightType: right.type });
      if (left instanceof BindingAst) {
        return new SetAst(left.type, SourceLocation.anon, left.binding, right);
      }
      if (left instanceof FieldAst) {
        return new SetFieldAst(VoidType, SourceLocation.anon, left.left, left.field, right);
      }
      compilerAssert(false, 'Not implemented AssignmentNode', { left, right });
    }

    if (node instanceof IdentifierNode) {
      const binding = this.getVariable(node.name);
      compilerAssert(binding, 'Variable not found', { ast: node });
      return new BindingAst(binding.type, SourceLocation.anon, binding);
    }

    if (node instanceof CreateStructNode) {
      const type = this.getType(node.name);
      const fields = node.fields.map(x => this.compile(x));
      // const binding = type.typeInfo.metaobject.constructorBinding as Binding
      // const call = new CallAst(type, SourceLocation.anon, binding, fields, [])
      // const letBinding = new Binding('temp', type)
      // const let_ = new LetAst(type, SourceLocation.anon, letBinding, null, true)
      // return new StatementsAst(type, SourceLocation.anon, [let_, call])
      return new ConstructorAst(type, SourceLocation.anon, fields);
    }

    if (node instanceof MemberExpressionNode) {
      const object = this.compile(node.object);
      const field = node.property;
      const type = object.type;
      const fieldIndex = type.typeInfo.fields.findIndex(x => x.name === field);
      compilerAssert(fieldIndex >= 0, 'Field not found', { object, field, type });
      return new FieldAst(type.typeInfo.fields[fieldIndex].fieldType, SourceLocation.anon, object, type.typeInfo.fields[fieldIndex]);
    }

    if (node instanceof FunctionDeclarationNode) {
      compilerAssert(!this.getFunction(node.name), 'Function already defined', { ast: node });
      this.scopeStack.push(new Scope());
      this.scope = this.scopeStack[this.scopeStack.length - 1];

      const concreteTypes = node.params.map(x => this.getType(x.type));
      const argBindings = node.params.map((x, i) => new Binding(x.name, concreteTypes[i]));

      argBindings.forEach((param, i) => {
        this.scope.variables[param.name] = param;
      });
      const body = this.compile(node.body);

      this.scopeStack.pop();
      this.scope = this.scopeStack[this.scopeStack.length - 1];

      const binding = new Binding(node.name, VoidType);
      const returnType = this.getType(node.returnType);
      const funcDef: FunctionDefinition = { debugName: node.name } as any; // Just need debugName for now

      const parameters = argBindings.map((argBinding, i) => {
        return createParameter(argBinding, node.params[i].capability);
      });

      const compiledFunc = new CompiledFunction(binding, funcDef, returnType, concreteTypes, body, argBindings, parameters, [], 0);
      this.scope.functions[node.name] = compiledFunc;
      this.allFunctions.set(binding, compiledFunc);

      return new VoidAst(VoidType, SourceLocation.anon);
    }

    if (node instanceof ReturnNode) {
      const value = node.argument ? this.compile(node.argument) : null;
      return new ReturnAst(VoidType, SourceLocation.anon, value);
    }

    if (node instanceof CallExpressionNode) {
      const func = this.getFunction(node.callee);
      compilerAssert(func, 'Function not found', { ast: node });
      const args = node.args.map(x => this.compile(x));
      const binding = func.binding;
      compilerAssert(binding && binding instanceof Binding, 'Binding not found', { ast: node });
      return new CallAst(func.returnType, SourceLocation.anon, binding, args, []);
    }

    if (node instanceof PrintNode) {
      const value = this.compile(node.value);
      if (value.type === IntType) return new CallAst(VoidType, SourceLocation.anon, externalBuiltinBindings.printInt, [value], []);
      return new CallAst(VoidType, SourceLocation.anon, externalBuiltinBindings.print, [value], []);
    }

    if (node instanceof IfStatementNode) {
      const test = this.compile(node.condition);
      const consequent = this.compile(node.consequent);
      const alternate = node.alternate ? this.compile(node.alternate) : null;
      return new IfAst(VoidType, SourceLocation.anon, test, consequent, alternate);
    }

    if (node instanceof AndNode) {
      const left = this.compile(node.left);
      const right = this.compile(node.right);
      return new AndAst(BoolType, SourceLocation.anon, [left, right]);
    }

    if (node instanceof WhileStatementNode) {
      const test = this.compile(node.condition);
      const body = this.compile(node.body);
      return new WhileAst(VoidType, SourceLocation.anon, test, body);
    }

    compilerAssert(false, 'AST Not implemented', { ast: node });

  }

  getType(typeName: string) {
    const constant = this.getConstant(typeName);
    compilerAssert(constant, 'Constant not found', { type: typeName });
    compilerAssert(isType(constant), 'Not a type', { type: typeName });
    return constant;
  }
}
