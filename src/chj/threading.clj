(ns chj.threading
    (:require [clojure.tools.reader :refer [syntax-quote]]
              [chj.util :refer [vector-cons cons* => either maybe]]))


(defmacro defn* [pred nam & binds&body]
  "Implicit context threading.

Writing `defn* foo?` instead of `defn` adds a first argument named
`this` to the defined function (for which foo? needs to be true), and
defines the function with an underscore prepended (`_nam`), and
defines a macro named `nam` which, when invoked, prepends `this` from
the current environment to the argument list.

This propagates `this` as an implicit variable similar to a dynamic
variable, but with a few differences:

 - using defn* adds an explicit visual cue that a \"dynamic\" context
   is involved, and calling the defined function (macro) without a
   `this` in context results in a compile time error (it means there's
   a broken thread -> less error prone)

 - closures and `lazy-seq` capture the value of `this` at creation
   time (-> functions remain pure)
"
  (=> symbol? nam)

  (let [_nam (symbol (str "_" nam))]
    
    `(do (defmacro ~nam [& args#]
           ;; `~~_nam and `~'~_nam don't work, thus use org.clojure/tools.reader
           (cons* (syntax-quote ~_nam) '~'this args#))
         ~(if (seq binds&body)
              (let [[binds & body] binds&body]
                `(defn ~_nam ~(vector-cons 'this binds)
                   ~@(if pred
                         `((=> ~pred ~'this)))
                   ~@body))))))

(defmacro def* 
  "Companion for `defn*`, for cases where `nam` is to be defined from
an expression; it just creates the wrapper macro, and depends on the
expression returning a function that takes `this` as the first
argument.

Note: def* ignores the pred argument, it's purely documentary."
  ([pred nam maybe-docstring expr]
   (=> symbol? nam)
   (=> (maybe string?) maybe-docstring)
   (let [_nam (symbol (str "_" nam))]
    
     `(do (defmacro ~nam [& args#]
            ~@(if maybe-docstring (list maybe-docstring) '())
            (cons* (syntax-quote ~_nam) '~'this args#))
          (def ~_nam
               ~@(if maybe-docstring (list maybe-docstring) '())
               ~expr))))
  ([pred nam expr]
   (=> symbol? nam)
   `(def* ~pred ~nam nil ~expr)))

