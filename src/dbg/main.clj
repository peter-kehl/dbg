(ns dbg
 (:require clojure.pprint))
 
; TODO macro that detects any ("string-literal" ...). Those are leftovers after removing dbg and similar from (dbg "string-literal description" ...) 
(def conj
  (if
    (or
        (> (:major *clojure-version*) 1)
        (> (:minor *clojure-version*) 4))
    clojure.core/conj
    (fn conj-compat
      ([coll] coll)
      ([coll & entries]
       (apply clojure.core/conj coll entries)))))

; different to org.clojure/tools.trace, because here we don't need extra parens ()

;TODO How to ensure the file is loaded as the first (or before a set of files), so that it re-defines 'fn' macro for them?
; -> future: redefine defn, fn

;BIG TODO: wrap everythin in with-out-str somehow, so it indents user's calls to print.
;TODO (time) - optional?
(defn dbg-show-function [value]
  (def ^:dynamic dbg-show-function-forms value))
(dbg-show-function false)

(def ^:dynamic dbg-indent-level 0)
(defn dbg-indent-plus [] (def ^:dynamic dbg-indent-level (inc dbg-indent-level)))
(defn dbg-indent-minus [] (def ^:dynamic dbg-indent-level (Math/max (dec dbg-indent-level) 0))) ;TODO warn on negative, but prevent further dbg-unindent reporting
(defn dbg-indentation [] (apply str (repeat dbg-indent-level "  ")))

;By default we don't indent the first line, so it can be appended to an existing content.
(defn dbg-indent
  ([content] (dbg-indent content false))
  ([content indentFirstLine]
   (let [indentedOtherLines (clojure.string/replace content "\n" (str "\n" (dbg-indentation)))]
     (if indentFirstLine
       (str (dbg-indentation) indentedOtherLines)
       indentedOtherLines))))

; Like clojure.core/print, but indented
(defn dbg-print [& args]
  ;alternatively: (binding [*out* ...] (callback...)) or (def *out* ....)
  (print (dbg-indent
           (reduce
             #(if (= % "")
                %2
                (str % \space
                  (if (nil? %2) "nil" %2)))
             "" args)
           true)))
(defn dbg-println [& args]
  (apply dbg-print args)
  (println)) ;Don't append "\n" to args of dbg-print, because it calls dbg-indent which removes a trailing newline.

; A helper to capture output of clojure.pprint/pprint. Remove an extra newline at the end.
(defn pretty [obj]
  ;Good that Clojure regex doesn't use Java Regex MULTILINE-like "m?" by default, because we want to exclude the last line only
  (clojure.string/replace (with-out-str (clojure.pprint/pprint obj)) #"\n$" ""))

(defn dbg-screen-columns
  ([] (dbg-screen-columns 150))
  ([mx]
   (println (loop [res "1"]
              (if (>= (count res) mx)
                res
                (recur (str res \space (+ (count res) 2))))))))

(def dbg-screen-width "Max. line width for dbg-pprint-last (when possible). If 0, no such limit." 80)

; Print a given message, indented, a space, and pretty print a given object. The object goes on the same line, or on a separate line.
; If the last line of message, once indented, the space and the pretified object, fit within dbg-screen-width, then
; this prints them on the same line. Otherwise it prints the pretified object (indented) starting on a new line. 
; Unlike clojure.pprint/pprint, this does *not* append a newline.
(defn dbg-pprint-last [prefix obj]
  (dbg-print prefix)
  (let [prefix-str (str prefix)
        prettified (pretty obj)
        obj-has-one-line (re-matches #"^.*$" prettified)
        ; To be platform-independent,
        ; using (.|\n) instead of (?s) which would make dot . also match newlines.
        ; (?s) is Java-specific but not for CLJS or .NET (https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html#DOTALL
        [_ prefix-bulk _ _ prefix-last] (re-matches #"^(((.|\n)*\n)*)([^\n]+)$" prefix)
        message-and-obj-fit-one-line
          (and
               obj-has-one-line
               (or
                  (zero? dbg-screen-width)
                  ;if a string, consisting of non-newline characters, exactly fills up a Linux KDE Terminal, and then you output a newline,
                  ;that newline works as expected: it starts a new line, but as if itself didn't have any width. I.e. it doesn't
                  ;generate two (visual) newlines. 
                  (<= (+ (count (dbg-indentation)) (count prefix-last) 1 (count prettified))
                      dbg-screen-width)))]
    (if message-and-obj-fit-one-line
      (print \space prettified)
      (do
        (println)
        (dbg-print \space prettified)))))

;---- For both dbg and dbgf.
(defn dbg-call-before [msg & args]
  ; Here and in dbg-*: Don't add colon : to printout, because it doesn't look good if msg is a keyword.
  (let [call-msg (str "Call " msg)]
    (if (seq args)
      (dbg-pprint-last (str call-msg " with") args)
      (dbg-print call-msg)))
  (println)
  (dbg-indent-plus))

(defn dbg-call-after [msg res]
  (dbg-indent-minus)
  (dbg-pprint-last (str "From " msg " return") res)
  (println))

(defn dbg-call-throw [msg e]
  (dbg-indent-minus)
  (dbg-println msg "Throw" msg "throwable:")
  (dbg-println e)
  (throw e))
  
;This is a macro and not a function, so we can use `dbg` with other macros/special forms.
;Otherwise users may need to wrap code in #(...) or (fn [] ....). THowever, that not only adds a set of parenthesis.
;It also upsets any (recur...) from inner code (until https://dev.clojure.org/jira/browse/CLJ-2235).
(defmacro dbg-call [msg & code]
  (list 'do
    `(dbg-call-before ~msg)
    (list 'try
       (list
          'let ['res code]
          (list 'dbg-call-after msg 'res)
          'res)
       (list
          'catch 'Throwable 'e
          (list 'dbg-call-throw msg 'e)))))

; an alternative to dbg-call, but it only works with functions, not with macros/special forms. Used by dbg.
(defn dbg-call-f [msg fun & args]
  ; Here and in dbg macro: Don't use colon : in printout, because it doesn't look good if msg is a keyword.
  (apply dbg-call-before msg args)
  (try
    (let [res (apply fun args)]
      (dbg-call-after msg res)
      res)
    (catch Throwable e
      (dbg-call-throw msg e))))

; If we need to treat a string into a symbol literal-compatible string. See https://clojure.org/reference/reader#_symbols

;https://clojure.org/guides/weird_characters
; - every time a particular x# is used within a single syntax quote, the _same_ generated name will be used.

;Invoke either
; - without a message: (dbg function args...), (dbg (function-expr) args...)
; - with a message as a string literal:
; - with a message as a keyword literal - the
(def dbg-snapshot-prefix "dbg-snapshot")

;TODO pprint of function expression < https://clojuredocs.org/clojure.pprint/pprint#example-5b950e6ce4b00ac801ed9e8a
; -- (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch (clojure.pprint/pprint (clojure.edn/read-string
;     "code-as-string-here"  )))
; Insert `dbg "description"` before function calls, like
; `(dbg "+ on numbers" + 1 2)`
; Beware of lazy sequences when tracing errors: for example, (for) creates a lazy sequence, hence callbacks will be delayed
; Details: Insert 'dbg' in front of most calls, except for:
; - special forms and macros. Wrap them in #(...) of (fn [] ...)
; - keyword literal serving as an accessor function, for example (:i {:i 1}). For them, either
; --- insert a string literal message (but not another keyword literal): (dbg ":i from a map" :i {:i 1}), or
; --- insert :_, followed by a keyword literal (to set a scope/reference for inner (dbg) calls) or :_. For example
;     (dbg :_ :i-from-a-map :i {:i 1}) or (dbg :_ :_ :i {:i 1})
; No need to insert anything in front of a symbol literal serving as an accessor function.
; For example ('i {'i 1}) => (dbg 'i {'i 1}). (Plus, any function names are also symbols, and we want them to work intuitively.)
(defmacro dbgf [msgOrFun & others]
  (let [firstKeyword (if (keyword? msgOrFun) msgOrFun)
        secondKeyword (if (and
                               firstKeyword
                               (keyword? (first others)))
                        (first others))
        msgAsGiven (or
                       (and (string? msgOrFun) msgOrFun)
                       (and (not= firstKeyword :_) (not secondKeyword) firstKeyword)
                       (and (not= secondKeyword :_) secondKeyword))
        ;firstIsNotFunction may be true even though msgAsGiven is nil, if keyword(s) are :_
        ;Can't do negative check for firstIsNotFunction, because a function may be represented by a symbol or a list (to evaluate)
        firstIsNotFunction (or (string? msgOrFun) (keyword? msgOrFun))
        ; "logical" (with a variable position among parameters):
        msg (or msgAsGiven (str &form)) ;without (str) the macro would inject the user's code unqouted
        ;TODO here and below: if (str &form), pprint as code & try on one line or two
        fun (if (not msgAsGiven)
              msgOrFun
              (if secondKeyword
                (second others)
                (first others)))
        args (if (and
                      (not (string? msgOrFun))
                      (not (keyword? msgOrFun)))
               others
               (if secondKeyword
                 (drop 2 others)
                 (rest others)))
        fun-expr (if (and
                          (not (symbol? fun))
                          (not (keyword? fun))) ;a keyword if accessing a map entry
                   (if firstIsNotFunction
                     (if secondKeyword
                       (str (nth &form 3))
                       (str (nth &form 2)))
                     (str (nth &form 1))))
        fun-holder (gensym 'fun-holder)
        scopeBackReferenceKeyword (if (and firstKeyword (not= firstKeyword :_) secondKeyword)
                                    firstKeyword)
        scopeForwardDefinitionKeyword (if firstKeyword
                                        (if secondKeyword
                                          (if (not= secondKeyword :_) secondKeyword)
                                          (if (not= firstKeyword  :_) firstKeyword)))]
    ;TODO 1. insteaf of keyword, use string. 2. treat string literal to be symbol-friendly
    ;3. prevent various literals (with special characters) that would lead to the same symbol
    ;4. prevent same literals (and hance same symbols) to override an upper scope.
    ;--- search in &env for a symbol with name equal to value dbg-scope-symbol and for any symbol starting with value of dbg-snapshot-prefix
    ;--- generate `[let ~dbg-scope-symbol (dbg-info :label ~user-string]
    (let [declare-binding (list 'binding ['dbg-indent-level
                                          (list 'inc (symbol (str dbg-snapshot-prefix scopeBackReferenceKeyword)))])
          declare-let (list 'let [(symbol (str dbg-snapshot-prefix scopeForwardDefinitionKeyword)) 'dbg-indent-level])
          execute (concat
                    (if (and
                             (not (symbol? fun))
                             dbg-show-function-forms)
                      (list 'dbg-println "Fn for" msg "<-" fun-expr)) ;dbg-println here helps us identify evaluation of the function-generating expression from running the function itself.
                    ;no need to pre-eval the function expression to call, because that is done as a part of calling dbg-call-f.
                    (list
                      (list 'let `[~fun-holder ~fun] ;let allows us to separate any logs of the function-generating expression from the targt function call.
                        (if (seq args)
                          (list 'dbg-println "Args for" msg))
                        (seq (apply conj ['dbg-call-f msg fun-holder] args)))))]
      (concat
        (if scopeBackReferenceKeyword 
          (concat
            declare-binding
            (if scopeForwardDefinitionKeyword
              (list
                (concat declare-let execute))
              execute))
          
          (if scopeForwardDefinitionKeyword
            (concat declare-let execute)
            (concat '(do) execute)))))))

;TODO put in a separate file, so it's comparable to dbgf
; Like dbg, but this works with either functions, macros or special forms. However, it doesn't print any arguments.
(defmacro dbg [msgOrFun & others]
  (let [firstKeyword (if (keyword? msgOrFun) msgOrFun)
        secondKeyword (if (and
                               firstKeyword
                               (keyword? (first others)))
                        (first others))
        msgAsGiven (or
                       (and (string? msgOrFun) msgOrFun)
                       (and (not= firstKeyword :_) (not secondKeyword) firstKeyword)
                       (and (not= secondKeyword :_) secondKeyword))
        ;firstIsNotFunction may be true even though msgAsGiven is nil, if keyword(s) are :_
        ;Can't do negative check for firstIsNotFunction, because a function may be represented by a symbol or a list (to evaluate)
        firstIsNotFunction (or (string? msgOrFun) (keyword? msgOrFun))
        ; "logical" (with a variable position among parameters):
        msg (or msgAsGiven (str &form)) ;without (str) the macro would inject the user's code unqouted
        code (if (and
                      (not (string? msgOrFun))
                      (not (keyword? msgOrFun)))
               (drop 1 &form)
               (if secondKeyword
                 (drop 1 others)
                 others))
        scopeBackReferenceKeyword (if (and firstKeyword (not= firstKeyword :_) secondKeyword)
                                    firstKeyword)
        scopeForwardDefinitionKeyword (if firstKeyword
                                        (if secondKeyword
                                          (if (not= secondKeyword :_) secondKeyword)
                                          (if (not= firstKeyword  :_) firstKeyword)))]
    (let [declare-binding (list 'binding ['dbg-indent-level
                                          (list 'inc (symbol (str dbg-snapshot-prefix scopeBackReferenceKeyword)))])
          declare-let (list 'let [(symbol (str dbg-snapshot-prefix scopeForwardDefinitionKeyword)) 'dbg-indent-level])
          execute (concat
                    (list
                      (seq (apply conj ['dbg-call msg] code))))]
      (concat
        (if scopeBackReferenceKeyword 
          (concat
            declare-binding
            (if scopeForwardDefinitionKeyword
              (list
                (concat declare-let execute))
              execute))
          
          (if scopeForwardDefinitionKeyword
            (concat declare-let execute)
            (concat '(do) execute)))))))

; TODO dbg>> for cross-thread keyword references
; TODO dbg-cfg macro

; Problem with code generated by a macro?
; 1. Run macro-expand, macroexpand-1 macroexpand-all (with the code quoted)
; 2. (clojure.pprint/with-pprint-dispatch clojure.pprint/code-dispatch (clojure.pprint/pprint 'generated-code-here ))

;replacement for skipping the dbg, but only for forms with a string message: 
;(defmacro dbg [one & others] (rest &form))

(if false
   (dbgf :out (fn[]
                (dbgf :out :in (fn []
                                 #_(println "in" dbg-indent-level)
                                 (dbgf :in :innermost #(println "innermost" dbg-indent-level)))))))
(if false
   (dbg :out (fn[]
               (dbg :out :in (fn []
                               #_(println "in" dbg-indent-level)
                               (dbg :in :innermost #(println "innermost" dbg-indent-level)))))))

(if false
  (dbg :let let [i 1] i))

(if false
  (dbg :out (fn[])
    (println "out" dbg-indent-level)
    (dbg :out :in #(println "in" dbg-indent-level))))
(if false
  (defn arity-test [[one two]])
  (if false (arity-test 0))) ;function arity checks are only done when evaluating
;but symbols are checked when compiling:
#_(if false (missing-function))
#_(if false (#(missing-function)))
(if false ((fn [par]) #_missing_par))

; The above dbg macro on its own doesn't work with recur. That's because recur requires tail recursion.
; Hence dbgr. It replaces tail recursion with non-tail. How? By:
; - redefining 'loop, 'fn and 'defn as new macros that
; --- create new functions with unique names
; --- define 'recur as a locally-bound function, that calls the user-created function (i.e. non-tail recursion).
; However, dbgr doesn't support letfn - too complicated (would you like to fork and implement that?). It can't support recur within #(), because hanling of #() can't ve overriden.
; --- put comments on separate lines, to allow merging
; Copy (re-indented) of original Clojure 1.10 implementations
; (https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj):
; TODO fork from CLJ; branch; remove other macros/functions, add own namespace - then merge their upstream changes
; --- even if my code refers to clojure.core/*, keep the copy in GIT, so that merge conflicts indicate their incompatible changes
; ^<--- Can Clojure have same module/package split in several files, even in several folders?
; An alternative: base on simpler implementation near the top of the same clojure/core.clj, but add (destructure). 
(defn ^{:private true}
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
           ~@body))))))

(defmacro fn-orig
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol
  
  Defines a function"
  {:added "1.0", :special-form true,
   :forms '[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}
  [& sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs)) 
               (list sigs) 
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (IllegalArgumentException. 
                          (if (seq sigs)
                            (str "Parameter declaration " 
                              (first sigs)
                              " should be a vector")
                            (str "Parameter declaration missing"))))))
        psig (fn* [sig]
               ;; Ensure correct type before destructuring sig
               (when (not (seq? sig))
                 (throw (IllegalArgumentException.
                          (str "Invalid signature " sig
                            " should be a list"))))
               (let [[params & body] sig
                     _ (when (not (vector? params))
                         (throw (IllegalArgumentException. 
                                  (if (seq? (first sigs))
                                    (str "Parameter declaration " params
                                      " should be a vector")
                                    (str "Invalid signature " sig
                                      " should be a list")))))
                     conds (when (and (next body) (map? (first body))) 
                             (first body))
                     body (if conds (next body) body)
                     conds (or conds (meta params))
                     pre (:pre conds)
                     post (:post conds)                       
                     body (if post
                            `((let [~'% ~(if (< 1 (count body)) 
                                           `(do ~@body) 
                                           (first body))]
                                 ~@(map (fn* [c] `(assert ~c)) post)
                                 ~'%))
                            body)
                     body (if pre
                            (concat (map (fn* [c] `(assert ~c)) pre) 
                              body)
                            body)]
                 (maybe-destructured params body)))
        new-sigs (map psig sigs)]
    (with-meta
      (if name
        (list* 'fn* name new-sigs)
        (cons 'fn* new-sigs))
      (meta &form))))

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                  (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
     ~(let [more (nnext pairs)]
        (when more
           (list* `assert-args more)))))

; From @seancorfield on Clojurians slack:
;  it doesn't use the result of calling `destructure` if that returns something other than its input argument.
; So the `bindings` being used in the code above would be the _original_ bindings, including the destructuring forms...
; ...so those won't be symbols, e.g., `(loop [a 1 {:keys [x y]} input] ...)`
; Here `a` is a `symbol?` but `{:keys [x y]}` is not.
; so you get a binding vector that has `[... g__123 input {:keys [x y]} g__123 ...]`
; -- in other words, it delegates the actual destructuring to the expanded code.
(defmacro loop-orig
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (let [db (destructure bindings)]
    (if (= db bindings)
      `(loop* ~bindings ~@body)
      (let [vs (take-nth 2 (drop 1 bindings))
            bs (take-nth 2 bindings)
            gs (map (fn [b] (if (symbol? b) b (gensym))) bs)
            bfs (reduce (fn [ret [b v g]] ;in CLJ source this used reduce1
                          (if (symbol? b)
                            ;g is the formal & given parameter name
                            (conj ret g v)
                            ;g is the formal, but generated, parameter name
                            (conj ret g v b g)))
                  [] (map vector bs vs gs))]
        `(let ~bfs
            (loop* ~(vec (interleave gs gs))
               (let ~(vec (interleave bs gs))
                  ~@body)))))))

;(macroexpand '(loop [zero 0 [one two] [1 2]] #_expression 1))
(let* [zero 0
       G__446 [1 2]
       vec__447 G__446
       one (clojure.core/nth vec__447 0 nil)
       two (clojure.core/nth vec__447 1 nil)]
  (loop* [zero zero G__446 G__446]
    (clojure.core/let [zero zero
                       [one two] G__446] #_expression 1)))

; Overview of the generated code:
#_(let [loop-initial-bindings-here]
       ( (fn generated-function-name [given-or-generated-symbols]
           body))
       given-or-generated-symbols)
(defmacro dbgloop-throwaway
  "Like standard loop, but this adds debugging on *every* iteration. Beware that it changes recur to be non-tail and stack-consuming. Beware that it breaks (recur...) in any inner (fn...) or #(...)."
  {:special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (let [db (destructure bindings)]
    (if (= db bindings)
      `(loop* ~bindings ~@body) ;why not use the below in all cases?
      (let [vs (take-nth 2 (drop 1 bindings))
            bs (take-nth 2 bindings)
            gs (map (fn [b] (if (symbol? b) b (gensym))) bs) ;a list of "formal" parameters received by loop & our generated funtion
            bfs (reduce (fn [ret [b v g]] ;in CLJ source this used reduce1
                          (if (symbol? b)
                            (conj ret g v) 
                            (conj ret g v b g))) ; g is the formal generated/helper parameter name
                  [] (map vector bs vs gs))]
        `(let ~bfs
            (loop* ~(vec (interleave gs gs))
               (let ~(vec (interleave bs gs)) ;~ tilda can apply to a (seq of seq...)
                 ~@body)))))))

; Impl. beware: recur in letfn, which (re)defines recur, doen't work (fully) as expected. If you have
; (letfn [ (f [] recur) (recur [] 1)] (f)) ; here (f) returns value of symbol recur, i.e. locally defined function recur
; but
; (letfn [ (f [] (recur)) (recur [] 1)] (f)) ;here (f) invokes itself via recur, i.e. ignoring locally defined function recur
; -- another proof is that the following fails with an unexpected argument to recur <- (recur) refers to f:
; (letfn [ (f [] (recur 1)) (recur [p] 1)] (f)) 
; -- the same with recur defined in (let) within the function:
; (fn f [] (let [recur identity] (recur 1))
(defmacro dbgloop
  "Like standard loop, but this adds debugging on *every* iteration. Beware that it changes recur to be non-tail and stack-consuming. Beware that it breaks (recur...) in any inner (fn...) or #(...)."
  {:special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (let [vs (take-nth 2 (drop 1 bindings))
        bs (take-nth 2 bindings)
        gs (map (fn [b] (if (symbol? b) b (gensym))) bs) ;formal parameter names - whether user-given or generated (for destructuring)
        bfs (reduce (fn [ret [b v g]] ;in CLJ source this used reduce1
                      (if (symbol? b)
                        (conj ret g v) 
                        (conj ret g v b g))) ; g is the formal generated/helper parameter name
              [] (map vector bs vs gs))
        fn-name (gensym "dbgloop")]
    `(let ~bfs
        (letfn
             [(~fn-name ~(vec gs)
                       (let ~(vec (interleave bs gs))
                           ~@body))
              (~'dbgrecur    ~(vec gs) ; without ~' you'll get an error: Can't let qualified name: <namespace>/dbgrecur
                       (dbgf ~fn-name ~@gs))]
    
           (dbgf ~fn-name ~@gs)))
    #_(let ~(conj bfs
              'recur `(fn recur ~(vec gs))
              (let ~(vec (interleave bs gs)))
              ~@body)
        (dbgf recur ~@gs))))
        
                   
                  

; TODO See https://clojuredocs.org/clojure.core/take#example-5bba8825e4b00ac801ed9eac > "sees no exception"

;(defmacro defn-orig [])

; TODO Can we import a namespace that defines fn, and use/require so it hides clojure.core/fn? 
; If so, we can Define fn referring to clojure.core/fn.
; AND/OR: defmacro dbgloop

;TODO dbg, dbgf, dbgloop: (re)define let [dbg-prefix-scope XXX] and check it in &env to automate scopes 














