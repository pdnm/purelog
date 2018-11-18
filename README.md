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

Enter
* `quit` to exit (when not searching).  
* `;` to find the next solution,
* `.` to terminate searching,
* `#` to print the number of solutions found,
* `$` to find the remaining solutions.

## Examples
`stack exec purelog-exe sample.pl`
* `equal(f(g(a,X),X), f(Y, b)).`
* `ancestor(X, Y).`
* `append(cons(a, cons(b, nil)), cons(c, cons(d, nil)), R).`
* `append(cons(a, cons(b, nil)), R, cons(a, cons(X, cons(c, cons(d, nil))))).`
* `reverse(cons(a, cons(b, cons(c, cons(d, nil)))), R).`
