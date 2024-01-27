# Iterators

Iterators don't exist in this language because you can just write a function in the 'internal iterator' style and it compiles to efficient code with no extra work.

Importantly break, continue and return are lexically bound so they work exactly how you'd expect. This makes it nice to make infinite iterators that you can pause later. See the [transducers section](./transducers.md) for high level operators.

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