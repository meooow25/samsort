# Benchmarks

Benchmarking done with GHC 9.6.3 -O and the [criterion](https://hackage.haskell.org/package/criterion)
library. See `Main.hs` for details on the test cases. The output CSV file
is at [`result/result.csv`](result/result.csv).

## Comparing with other libraries

| Label | Library | Function |
| --- | --- | --- |
| `ssArray` | `samsort` | `Data.SamSort.sortArrayBy` |
| `ssIntArray` | `samsort` | `Data.SamSort.sortIntArrayBy` |
| `vaHeap` | `vector-algorithms` | `Data.Vector.Algorithms.Heap.sortBy` |
| `vaIntro` | `vector-algorithms` | `Data.Vector.Algorithms.Intro.sortBy` |
| `vaMerge` | `vector-algorithms` | `Data.Vector.Algorithms.Merge.sortBy` |
| `vaTip` | `vector-algorithms` | `Data.Vector.Algorithms.Tim.sortBy` |
| `ps` | `primitive-sort` | `Data.Primitive.Sort.sortMutableBy` |

An `-i` suffix indicates that the sort was done by index.

### 1. Sort $10^5$ `Int`s (boxed)

![Int](https://github.com/meooow25/samsort/assets/13716304/5d6887f6-c5a6-4a83-b173-49885d09e48d)

### 2. Sort $10^5$ `Maybe Int`s (boxed)

![MaybeInt](https://github.com/meooow25/samsort/assets/13716304/8233588b-0648-42f8-be04-9a1e86d5f537)

### 3. Sort $10^5$ `Int`s (unboxed)

![IntPrim](https://github.com/meooow25/samsort/assets/13716304/e8fd8bc6-f89c-4d34-ba9d-5b9aae346c64)

### 4. Sort $10^5$ `(Int, Int, Int)`s (unboxed)

![Int3Unbox](https://github.com/meooow25/samsort/assets/13716304/fde8526e-10a6-4ed0-8edd-22d7f54a39c0)

### 5. Sort $10^5$ `Word8`s (unboxed)

![Word8Unbox](https://github.com/meooow25/samsort/assets/13716304/658b0374-7394-48a2-a748-13956bfb3cf0)

