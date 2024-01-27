# Compile-time Transducers

The compiletime blocks and internal iterators features combine together and allow you to construct something I'm calling compile-time transducers which I've never seen done before. They enable something like Python for-comprehension but better. They a way to compose together small units of processing an run through iterables like lists or dictionaries. They work with iterators so they are general for any iterable including infinite ones.

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

Compiled C code (in the past the Radius compiler output the below C but nowadays it generates its own bytecode, but it is basically the same as below)
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

### Generators

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