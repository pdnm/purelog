# purelog
Core Prolog interpreter.

## Build
```
stack init
stack build
```

## Run
```
stack exec purelog-exe <code.pl>
```

During idle:
* `quit` to exit
* `strategy(BFS).` to switch to BFS (default is DFS)
* `strategy(DFS).` to switch to DFS

During search:
* `;` to find the next solution
* `.` to terminate searching
* `#` to print the number of solutions found
* `$` to find the remaining solutions

## Examples
`stack exec purelog-exe sample.pl`
* `equal(f(g(a,X),X), f(Y, b)).`
* `ancestor(X, Y).`
* `append([a, b], [c, d], R).`
* `append([A, b], [c, D], [a, B, C, d]).`
* `reverse([a, b, c, d], R).`
