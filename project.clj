(defproject dbg "0.1.2-SNAPSHOT"
  :description "Tracing evaluation order. It logs entering and leaving functions or macros/special forms. It indents them, along with any inner output - simple and pretty. You prefix function calls with `dbgf` or macros/special forms with `dbg`. It works without any extra parens (). Hence diffs are shorter, easier to read. Isn't that practical?"
  :url "https://github.com/peter-kehl/dbg"
  :license {:name "GNU GPL v. 3"
            :url "https://www.gnu.org/licenses/gpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]])
