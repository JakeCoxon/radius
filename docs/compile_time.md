# Compile-time code

Constant declarations are written with `::` and are evaluated at compile-time

```rust

fn my_thing() {
  const my_const = 3 + 2
  const constant = create_constant(4, 5)
  print(constant)
}

fn create_constant(x, y) {
  300 + x * y
}
```

---


The `meta if` and `meta for` statements execute the condition at compile-time, but the body is injected as runtime code. 

```rust

fn some_foo() {
  2 + 2
}

fn main() {
  meta if some_foo() == 4 {
    print("some_foo was 4")
  } else {
    print("some_foo was not 4")
  }
}
```

In this program only the first print will be compiled into the program.

---

```rust

const my_const_list = [1, 2, 3]

fn main() {
  meta for x in my_const_list {
    print(x * 10)
  }
}
```

In this program there will be 3 print statements compiled into the main function

---

Type parameters are just values, so any value can be passed at compile time. foo will be compiled to print 100 directly

```rust
fn foo!(T)() {
  print(T) // as if it was written print(100)
}

fn main() {
  foo!100()
}
```

This means a function can emit different code depending on it's type parameters, or any compile-time value

```rust
fn my_function!(T)(param: T) {
  meta if T == int {
    print("my int is", param)
  } else {
    print("another value is", param)
  }
}

fn main() {
  my_function(200)   // T is int
  my_function("foo") // T is string
}
```

---

Functions can also be passed as type parameters and used like regular. This is a limited version of higher-order functions that aren't closures
 
```rust
fn my_function!(f)() {
  f(1)
  f(2)
  f(3)
}

fn main() {
  const foo = {|x| print(x)}
  my_function!foo()

  // Prints 1, 2, 3
}
```

---

In the future more information about types and more AST operations will be available