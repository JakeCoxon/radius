# Compile-time closures

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