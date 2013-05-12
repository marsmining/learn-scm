## Notes while reading The Reasoned Schemer

### Summary

My notes following [The Reasoned Schemer](http://www.amazon.com/Reasoned-Schemer-Daniel-P-Friedman/dp/0262562146). Code is [here (src/reasoned.scm)](https://github.com/marsmining/learn-scm/blob/master/src/reasoned.scm).

### Preface

This is my second try reading this book. My first attempt ended maybe a quarter of the way through. I found it hard because a lot of the code was not like that of [The Little Schemer](http://www.amazon.com/Little-Schemer-Daniel-P-Friedman/dp/0262560992) where you could incrementally run the code. Or maybe I was doing something wrong. Since my first attempt, I've gone back and re-read the latter chapters of [The Little Schemer](http://www.amazon.com/Little-Schemer-Daniel-P-Friedman/dp/0262560992) more meticulously.

### Ch. 1 - Playthings

Ok so we're introduced to this `run*` fn or macro.

```scheme
(run
 #f         ; not sure what this is
 (q)        ; query
 (== #t q)  ; goal, unify true with query
 )          ; => (#t) returns one answer
```