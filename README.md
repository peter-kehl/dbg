# dbg

A Clojure library designed to trace evaluation of functions calls and forms. It indents any output.

# Usage

```
(dbgf function-name-or-function-generating-expression parameters-if-any...)
(dbg  macro-or-special-form-or-function-name parameters-if-any...)
```

## dbgloop and dbgrecur
Those serve to replace `loop` and `recur`. However, in order to indent any output, they don't use tail recursion. Instead, they use stack. Hence they may run out of stack space for long iterations.
 
```
(dbgloop [initial bindings] forms...)
(dbgrecur [new binding values])
```

[![Clojars Project](https://img.shields.io/clojars/v/dbg.svg)](https://clojars.org/dbg)
