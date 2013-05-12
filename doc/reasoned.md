## Notes while reading The Reasoned Schemer

### Summary

My notes following [The Reasoned Schemer](http://www.amazon.com/Reasoned-Schemer-Daniel-P-Friedman/dp/0262562146). Code is [here (src/reasoned.scm)](https://github.com/marsmining/learn-scm/blob/master/src/reasoned.scm).

### Preface

This is my second try reading this book. My first attempt ended maybe a quarter of the way through. I found it hard because a lot of the code was not like that of [The Little Schemer](http://www.amazon.com/Little-Schemer-Daniel-P-Friedman/dp/0262560992) where you could incrementally run the code. Or maybe I was doing something wrong. Since my first attempt, I've gone back and re-read the latter chapters of [The Little Schemer](http://www.amazon.com/Little-Schemer-Daniel-P-Friedman/dp/0262560992) more meticulously.

### Ch. 1 - Playthings

Ok so we're introduced to this `run*` fn or macro.

```scheme
(run
 #f         ; not sure what this is, typo?
 (q)        ; logic variable
 (== #t q)  ; goal, unify true with query
 )          ; => (#t) returns one answer
```

Then we're introduced to `fresh` which looks like `let`. Again, can't run any code :(. I know this book has something to do with [miniKanren](https://github.com/miniKanren/miniKanren), so gonna try to use that to run the code the book is introducing. Wait, seems book is based on Kanren, and seems co-author Oleg maintains code from book here: http://kanren.sourceforge.net/

So grabbing code from there, we can do:

```scheme
(load "kanren-book/mk.scm")
(load "kanren-book/mkextraforms.scm")
(load "kanren-book/mkprelude.scm")

(run*
 (q)
 (== #t q)) ; => (#t)
```

And it works! Can run the examples now while reading.