

<br />
<div align="center">
  <a href="https://github.com/JakeCoxon/radius">
    <img src="./docs/logo.svg">
  </a>

  <h3 align="center">Programming language</h3>

  <p align="center" style="padding:0 100px">
    Experimental, statically typed, low level language with high level features. I want to use it for graphics and audio. But expect almost nothing to work yet.
    <br />
  </p>
</div>


```rust

fn main() {
  let my_list = [1, 2, 3, 2, 1]
  for x in my_list {
    print(foo(x))
  }
}

fn foo(x: int) -> int {
  x * 32
}
```

## Current features

** Very work-in-progress - do not use **

* No external dependencies (except bun runtime)
* Static typing with generics - Sizes of structs are known at compile time so can compile to efficient staticly typed bytecode. One-way type inference, no automatic type conversions (except number literals)
* [Memory safety](docs/memory.md) with no garbage collection, no pointers, no manual memory management using compile-time exclusivity checking (similar to Rust borrow-checking but without the lifetime annotations)
* Compiles to LLVM IR textual format which can then be compiled to various format - Web Assembly, native code etc
* Dynamic arrays and strings with size field and bounds checks
* Functions are templated and monomorphise to independent functions like C++ templates (rather than Java generics)
* Out of order compilation - implemented using a custom task system and queue where tasks can be paused and resumed
* [Experimental web debugger](docs/debugger.md) - which lets you step through the compilation of a program and inspect every data struture used by the compiler, including the AST, compiled bytecode etc
* [Compile-time closures](docs/closures.md) - Closures that are firstclass at compile-time and are inlined at the callsite. They can be composed and passed around to compile-time functions
* [Iterator functions](docs/iterators.md) - Very simple to implement, customisable internal-iterator functions that is used by for-loop and has no runtime overhead.
* [Zero-overhead transducers](docs/transducers.md) - Composable, extensible, higher-order array operations with zero-overhead and fairly simple interface.
* [Metaprogramming AST](docs/compile_time.md) - AST manipulation at compile-time without macros, implemented using a builtin bytecode VM with instructions to create and manipulate AST objects
* [Vector value types using metaclasses](docs/metaclasses.md) - for 3d math with basic operator overloading
* [Built in windowing and graphics library](docs/graphics.md)
* Shorthand array operations - syntax sugar for mapping, zipping and reducing without higher order functions



## Future goals / ideas

* Codegen targets such as Web Assembly, Javascript and GLSL can be implemented as a library function
* Manual memory management with no garbage collection. But try and avoid footguns by ensuring bounds checking and memory arenas. Maybe disallow pointer arithmetic completely. Maybe optional ref counting for in debug mode that can be completely removed in release mode
* Pattern matching of some kind
* The language doesn't have traditional function overloading but I want to include some higherorder function utilities.
* No OOP, but some kind of interfaces/traits system

## Compile time evaluation

Why is compile time evaluation so important? It can replace many features from other languages into a simpler system

* Configure how to integrate with the outside environment and platform
* Configure how to compile the program, with different compiler options based on environment
* Understand and modify types, classes, fields etc
* Reduce code repetition
* Emit code based on compile time information (including command line options)
* Type checking helpers
* Library and standard library functions that can be configured with no runtime overhead or external build tools
* Produce statistics, visualisations, debug info about program during compilation
* Functional programming utilities
* Metacompile function definitions as function calls for potental speed?
* Compile-time closures
* Metaobject/metaclasses to customise object behaviour without any runtime overhead
* Editor/debugger integration

These things are written in the same language as regular code, so can they can use the same standard library functions that regular code uses

---

Number literals have a special literal type that can be coerced to whatever type is needed

```rust
let foo = 2 + 3 // int
let bar: float = 2 + 3 // float
let baz = 3.1 + 1.2 // float
```

But non-literals must be cast manually

```rust
let foo: int = 2
let bar: float = float(foo) + 2.1
```

This gives the best of both worlds where all types are checked, implicit conversions don't mess you up, and literals don't need to be annotated.

---

Function type parameters are defined using exclamation point

```rust
fn my_function!(T)(param: T) {
  ...
}

fn my_other_function!(A, B)() {
  ...
}

fn main() {
  my_function(100) // template params are inferred

  // If one type argument is passed then it doens't need a parenthesis
  my_function!int(100) 
  my_other_function!(f, 200)() // any compile time values can be passed
}
```

The same goes for type parameters

```rust
define MyThing!(T) {
  var foo: T
}

fn main() {
  let thing = MyThing!int(123)
}
```

---

In Radius the type checking rules are kept extremely simple to keep implementation simple. Combined with the fact that the last statement in a block is used as the result, it's difficult to type check an `if` used as expression.

The solution to that is an `ifx` keyword that enforces two branches that resolve to the same type. This can be used normally as multi line, or single line expression

```rust
fn main() {
  // Regular if statement doesn't need an else branch
  if something() {
    print("OK")
  }

  let foo = ifx something() { 2 } else { 3 }
  let bar = ifx something() {
    let x = 3
    let y = 8
    something_complicated(x, y)
  } else {
    something_else()
  }
}
```

## Development

It's at early stage so I wouldn't suggest using it properly yet.

* Requires [Bun](https://bun.sh/)
* Install with `bun install`
* Run tests with `bun test`
* Experimental web interface using `bun web/server.ts` (web-experiment branch)
