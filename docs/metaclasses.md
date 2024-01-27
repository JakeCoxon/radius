# Metaclasses 

This hasn't been fleshed out yet but there are vector types that use a VecType metaclass. The VecType is a metaclass (not object inheritance) that is executed at compile time and automatically produces the correct operators.

Currently VecType is implemented in the compiler but in the future it will be possible to write this in library code.

```python
type vec2i < VecType:
  x: int
  y: int

type vec3i < VecType:
  x: int
  y: int
  z: int

type vec2f < VecType:
  x: float
  y: float

type vec3f < VecType:
  x: float
  y: float
  z: float

fn main():
  v1 := vec2i(1, 2)
  v2 := vec2i(3, 4)

  # Vec-Vec operators are implemented automatically
  v3 := v1 + v2
  v4 := v2 - v1
  v5 := (v2 * v1) / (v1 + v2)

  v10 := vec3i(1, 3, 5)
  v11 := vec3i(7, 9, 11)
  v12 := v10 + v11

  v1 += vec2i(10, 10)
  v1 += (10, 20) # Tuple type is also supported
```