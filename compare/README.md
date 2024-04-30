## Comparison with other sorts

> `TODO: Update with latest data`

Benchmarking done with GHC 9.6.3 -O and the [criterion](https://hackage.haskell.org/package/criterion)
library. See `Main.hs` for details on the test cases. The output CSV file
is at [`result/result.csv`](result/result.csv).

### Large, sort $10^5$ `Int`s
![100000](https://github.com/meooow25/samsort/assets/13716304/81ec7063-d17d-4ade-925c-98418a3e90e5)

### Small, sort $10$ `Int`s
![10](https://github.com/meooow25/samsort/assets/13716304/9bd428f6-ba73-4c4e-87e5-8eb2c29ccac2)
