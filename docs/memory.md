# Memory safety

This language has no garbage collection, no pointers, no manual memory management. Instead it uses compile-time exclusivity checking to ensure that no two bindings are aliased.

All mutable and immutable access is tracked and checked at compile time.

This is similar to Rust's borrow checker but without the lifetime annotations.

It uses a custom intermediate representation and multiple passes to track the exclusivity of variables and insert the necessary memory barriers.

This technique is called mutable value semantics, since an object can not hold any references to another object. Instead it is a value that can be copied around. Since objects only own other objects it means that lifetimes are simple and no lifetime annotations are needed.

In order for objects to temporarily give access we use 'subscripts' in which the object forwards immutable/mutable access to a part of the original object. This is done in a way that doesn't 'return' access but defines a region of access that is checked at compile time.

## References

Using the following references to help implement this:

* [Hylo-lang](https://github.com/hylo-lang/hylo)
* [Borrow checking in Hylo](https://2023.splashcon.org/details/iwaco-2023-papers/5/Borrow-checking-Hylo)
* [Implementation Strategies for Mutable Value Semantics](https://www.jot.fm/issues/issue_2022_02/article2.pdf)
