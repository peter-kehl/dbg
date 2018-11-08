# dbg

A Clojure library designed to trace evaluation of functions calls, macros and special forms. It indents any output.

It logs entering and leaving functions or macros/special forms. It indents them, along with any inner output - simple and pretty. You prefix function calls with `dbgf` or macros/special forms with `dbg`. No need to add extra parens () around, hence this is practical.

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
