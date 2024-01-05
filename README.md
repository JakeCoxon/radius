

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


```python

fn main():
  my_list := [1, 2, 3, 2, 1]
  for x in my_list:
    print(foo(x))

fn foo(x: int) -> int:
  x * 32
```

## Design goals / ideas

* Small source code around 3000 lines. The core of it requires no node_modules. Hand written parser
* Compiler is responsible for a few things: parsing, type checking, intermediate representation, some sort of bytecode execution (for now JS).
* Compile time evaluation is primary feature for programs to communicate with the compiler
* As much as possible should be a library function written in the language or as a library function to keep core small
* Codegen targets such as Web Assembly, Javascript and GLSL can be implemented as a library function
* Static type checking. Try and keep implementation simple. Stronger type system than C. Probably leverage compile time evaluation. Simple type inference where types flow from leafs up the tree (with some minimal exceptions)
* Functions are templated and monomorphise to independent functions like C++ templates (rather than Java generics)
* Do not deviate from familiar syntaxes too much. For now similar to Python with some extra stuff. D-style template parameters are the weirdest part. I'm not dead set on indentation based syntax, I just find it slightly nicer to read and write
* I need math vectors and matrices and they must be value types
* Manual memory management with no garbage collection. But try and avoid footguns by ensuring bounds checking and memory arenas. Maybe disallow pointer arithmetic completely. Maybe optional ref counting for in debug mode that can be completely removed in release mode
* Arrays and strings must have a length field and bounds checked
* Library defined compile time functional utilities such as static dispatch, partial function application, function composition, list comprehension, transducers. Try and do this without closures
* Pattern matching of some kind
* No function overloading like other languages have, but with alternatives
* No syntax macros. But inlined closures cover most of the cases
* No OOP, but interfaces/traits system

## Implemented features

* Class definitions
* Basic operator overloading
* Type system with generic types
* Templated functions
* Function inlining
* Expression-based to statement-based rewriting. Makes macros and inlining work easily. This happens in the same pass as code generation
* Binding name remapping to avoid shadowing
* Compile-time bytecode compiler and interpreter
* Compile time meta compiler. ASTs are simply values that can be manipulated and written out to the target
* Compile-time closures for transducers with hygenic access to bindings including a hygenic break/continue out of lexical scope using first-class break and continue bindings
* Compile-time function execution

<div align=center style="background:white"><img src="./docs/files.png" width=400></div>

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
* Macros to add syntax (Probably not worth it to be honest)
* Functional programming utilities
* Metacompile function definitions as function calls for potental speed?
* Compile-time closures
* Metaobject/metaclasses to customise object behaviour without any runtime overhead
* Editor/debugger integration?

These things are written in the same language as regular code, so can they can use the same standard library functions that regular code uses

## Weird syntax

Type parameters are using exclamation point

```python
fn my_function!(T)(param: T):
  ...

fn my_other_function!(A, B)():
  ...

fn main():
  my_function(100) # template params are inferred
  my_function!int(100) # or passed explicitly
  my_other_function!(f, 200)() # any compile time values can be passed
```


## Compile-time code

Constants are evaluated at runtime
```python
fn create_constant():
  300 + 400 + 500

fn my_thing():
  constant :: create_constant()
  print(constant)
```

Type parameters are just values, so any value can be passed at compile time. foo will be compiled to print 100 directly

```python
fn foo!(T)():
  print(T) # as if it was written print(100)

fn main():
  foo!100()
```

This means a function can emit different code depending on it's type parameters, or any compile-time value

```python
fn my_function!(T)(param: T):
  meta if T == int:
    print("my int is", param)
  else:
    print("another value is", param)

my_const_list :: [1, 2, 3]

fn main():
  meta for x in my_const_list:
    print(x * 10)
```

## Compile-time closures

Functions and lambda functions will not be first class in this language. Instead it will have compile-time closures that are more restrictive than regular closures in other languages.

Compile-time closures are effectively templated lambda functions that can close over compile-time values. These values are constants or bindings to runtime values (rather than values themselves). This means closures can compose with other closures, but only during compile-time. When a closure is finally invoked the closure is instantiated and inlined and compiler can check that any refered binding is directly accessible in the lexical scope at the point of instantiation.

This means a binding in your program can escape the lexical scope into a library function and back to the original scope with no problems.

This restriction means closures don't have to allocate any memory on the heap, because every value is guaranteed to live as long as the closure itself. It also means than some forms of functional programming will not be possible, but you still gain the ability to compose high-level behaviours together with confidence about runtime performance and memory allocation. This will be useful in environments where dynamic memory allocation is not available (GPU shaders).

Compile time closures can also be thought of a more restricted macro expansion. Like macros they represent units of code that can be inlined in different callsites. But unlike macros they are guaranteed to not be modified from their original semantic meaning. They are fully hygenic so they mean the same thing no matter what context they are inlined into. This means compile-time closures can replace some usages of macro expansion in other languages such as C X-Macro which is just a way to parameterise multiple units of code.

Basic example
```python
# Higher order function is only executed at compile-time that return a lambda that will be inlined into runtime code
fn create_adder(x: int) any:
  {|y| x + y}

add_to_100 :: create_adder(100) # 100 is a constant value

fn main():

  input := get_user_input()
  
  # input can be passed to create_adder but it is a binding to input,
  # rather than a value itself (because it runs at compile time)
  add_to_user_input :: create_adder(input)

  my_constant :: 23 # Constant value

  # Constant values can be inlined no problem
  print(add_to_100(my_constant))

  # add_to_user_input refers to input binding, so input binding is
  # confirmed to be lexically scoped
  print(add_to_user_input(4))
```

```c
int main(void) {
  int input = get_user_input();
  print((100 /* x */ + 23 /* my_constant */));
  print((input + 4));
}
```

Another example would be the following. It works because reduce will be inlined and the contents of create_adder can directly refer to the list elements. The dream is to allow composing of transducers, parser combinators, type-level builders etc.

```python
list := [1, 2, 3]
new_list = reduce!create_adder(list)
```

## Iterators

Iterators don't exist in this language because you can just write a function in the normal way and it compiles to efficient code with no extra work. 

Importantly you can break, continue and return out of closures (since they are just inlined). This makes it nice to make infinite iterators that you can pause later. See the transducers section later for high level operators.

Here is an example of a custom iterator over a 2d grid. The break and continue target is lexically scoped so it knows to break out of *both* while loops, so it just works how you would expect.

```python
type Range2d:
  w: int
  h: int

  # This tells the compiler what function to use for iteration
  __iterate :: range2d_iterate

# the loop body is passed as a template parameter 'f'. and the function must be marked @inline
fn range2d_iterate!(f)(range: Range2d) void @inline:
  for j in Range(0, range.h):
    for i in Range(0, range.w):
      f(vec2i(i, j))

type Range:
  start: int
  end: int
  __iterate :: range_iterate

fn range_iterate!(f)(range: Range) void @inline:
  i := range.start
  while i < range.end:
    f(i)
    i += 1

type vec2i:
  x: int
  y: int

fn main():
  range := Range2d(32, 64)
  for vec in range:
    if vec.x * vec.y > 256:
      continue # continue does the right thing
    print(vec.x, vec.y)

```

## Compile-time Transducers

The above features together combine into something I'm calling compile-time transducers which I've never seen done before. They enable something like Python for-comprehension but better. They a way to compose together small units of processing an run through iterables like lists or dictionaries. They work with iterators described above so they are general for any iterable including infinite ones.

* They don't create intermediate arrays when composing together
* In fact they may not allocate any array at all if all you want is a single result such as the average or the maximum of an array after processing.
* There is no overcalculation when composing maps, filter, take etc. Only the values needed will be computed
* They are fully extensible - They are all implemented in library functions and the user can extend with their own iterators, transducer functions and reducers. Only a syntax is built into the language to make things nicer but everything still works without it.
* There is no optimisation in this compiler so you don't have to wonder if the output code will be efficient. They are semantically guaranteed to compile to simple code that you might have written by hand. No closures or extra dynamic allocation happens at runtime.

```python
import transducers for compose, map, sum

defn main():

  # You can compose together transducers using standard function
  # composition into a single transformation
  transform :: compose(
    map {|x| x * 100},
    map {|x| x + 1}
  )

  my_list := [1, 2, 3, 2, 1]

  # Run the transducer and use sum() as the reducer
  res := [my_list... | transform |> sum()]

  # or chain them directly:
  [my_list... | map {|x| x * 100} | map {|x| x + 1} |> sum()]

comptime:
  compile_c(main)

```


```c
void main_1(void) {
  int _0[] = {1, 2, 3, 2, 1};
  slice_t__int my_list = make_slice(_0, c_array_size(_0));
  int acc = 0;
  int i = 0;
  while ((i < my_list.size)) {
    int _1 = my_list.data[i];
    int _2 = (_1 + 1);
    int _3 = (_2 * 100);
    acc = (acc + _3);
    labelblock_continue:
    i = (i + 1);
  }
  labelblock_break:
  int res = acc;
}

```

In this example you can see that the transducer closes over a binding to a runtime value with no problem.
```python
i := 0
transform :: compose(
  map {|x|
    i += 1
    x * i
  },
  filter {|x| x > 10}
)
res := [my_list... | transform |> sum()]
```

### Iterators

(Not implemented yet) You will be able to use subset of lists, combine lists together and also use infinite generator functions
```python
res := [my_list[1:]... |> sum()]

res := [(my_list[:] + my_list[1:])... |> sum()]

res := [1, 2, 3, one_list..., two_list... |> sum()]

res := [iterate(my_generator())... |> sum()]
```

### Reducers

Reducers say how to combine the results into a single value. This could be a new array, an existing array, a single number, or any other data structure.

```python
# By default it will map every item into a new array that may need to allocate. But you can change it to write each item into a new fixed size array, or a growable array
res := [my_list... | map {|x| x * 100}]
res := [my_list... | map {|x| x * 100} |> into_new_array()]

# Preallocate a fixed-size array and stop when it gets to the end (not implemented yet)
output_array := create_fixed_array(10)
[my_list... | map {|x| x * 100} |> into_fixed_array(output_array)]

# Push into an existing array (not implemented yet)
output_array := []
[my_list... | map {|x| x * 100} |> push_into_array(output_array)]
```

Some reducers reduce into a single value that's not a collection of data. For example if you just want the min, max, average, first, last then you can easily use these reducers and it won't have to allocate the intermediate array

```python
my_list_3 := [1, 2, 3, 4, 5, 4, 3, 2, 1]
f :: filter {|x| x < 4}
list_max := [my_list_3... | f |> max()] # 3
list_average := [my_list_3... | f |> mean()] # 2
list_first := [my_list_3... | f |> first()] # 1
```

Also note in the last example that we compose the filter f but we just take the first value, which exits early so the filter is only applied to one element.

### Basic transducers

```python
# take, take_while, 
# Others include: drop, takeLast, dropLast, drop_while transducers (not implemented yet)
res := [my_list... | take(5)]

res := [all_ints()... | map {|x| some_function(x)} | take_while {|x| x < 1000}]
```

We can also include other transducers for splitting the input in different ways eg: sliding window, partition, chunking, pagination.


## Future work

Because we have given the consumer the control of the callback, the approach doesn't work well for more advanced iterators that need to use the callstack (such as tree traversal). In addition it's difficult to zip, interleave and even chain arbitrary iterators, because again the producer needs control of the callstack. See https://journal.stuffwithstuff.com/2013/01/13/iteration-inside-and-out/

I really think this language needs to support this, without losing the flexibility of writing internal iterations, and maintaining the ability of the consumers to control the stack. It should be possible to transparently rewrite an internal iterator in external style that stores necessary stack state in a struct. Apparently this is what C# does. I think this is different from fibers because instead of creating an entirely new stack the original callframe owns the data. There is a memory and performance overhead of this so it should be explicitly opt-in for the user. If you're working with lists or other data structures that have random access then this shouldn't be necessary and it should just work.

## Running

* Requires `bun`
* Install with `bun install`
* Run tests with `bun test`
* Experimental web interface using `bun web/server.ts`
