# samsort

A stable adapative mergesort implementation

## Features

This is a lightweight library offering a high performance primitive sort
function. The function sorts a GHC
[`MutableArray#`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exts.html#t:MutableArray-35-)
in place.

There are no dependencies outside of `base`. This means that this library is
not tied to array abstractions from any particular library. This also means
that you may need to write a wrapper function that sorts your flavor of Haskell
array, such as ones from
[`primitive`](https://hackage.haskell.org/package/primitive-0.9.0.0/docs/Data-Primitive-Array.html#t:MutableArray),
[`vector`](https://hackage.haskell.org/package/vector-0.13.1.0/docs/Data-Vector-Mutable.html#t:MVector),
[`array`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Arr.html#t:STArray),
or elsewhere. You can find an example with `primitive`
[here](https://github.com/meooow25/samsort/blob/master/compare/Main.hs#L61-L64).

If you need to use this library in an environment where you cannot depend on
other packages, you may simply copy the lone source file
[`src/Data/SamSort.hs`](https://github.com/meooow25/samsort/blob/master/src/Data/SamSort.hs)
to your project.

## Algorithm details

* The sort is a comparison-based $O(n \log n)$ mergesort.
* The sort is stable, i.e. the order of equal elements in the input is
  preserved.
* The sort is adaptive, i.e. the sort identifies and uses ascending and
  descending runs of elements occuring in the input to perform less work. As a
  result, the sort is $O(n)$ for already sorted inputs.
* The performance is comparable to and in many cases better than the comparison
  sorts from the [vector-algorithms](https://hackage.haskell.org/package/vector-algorithms)
  library. See [the benchmarks](https://github.com/meooow25/samsort/tree/master/compare)
  for details.

## FAQ

#### Why not use \<insert strategy\>?

I'm open to changing the implemention if an alternative is demonstrated to
perform better, as long as the sort remains stable and adaptive.

#### How do I sort an unboxed array with this library?

You can't. To sort different types of arrays, I would have to rely on an
existing library's abstractions (like `vector-algorithms` relies on `vector`),
or roll my own. This goes against the goal of keeping the library lightweight. I
do not have a solution to this problem at the moment.
