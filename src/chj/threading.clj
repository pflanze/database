(ns chj.threading
    (:require [clojure.tools.reader :refer [syntax-quote]]
              [chj.util :refer [vector-cons cons*]]))


(defmacro defn* [nam & binds&body]
  "'Automatic' context threading for tree ops.

Like defn (but only supports the single-definition form), but
defines the function with nam prefixed with an underscore and an added
first `_tree-ctx` argument, and defines a macro under the name nam
which adds `_tree-ctx` (unhygienically) as the first argument.

Note: as with anything that uses unhygienic bindings, this feels
slightly dirty. It's straight-forward enough for the limited scope of
intended use, though, and a clean approach (gensym and then code
walker? Communicate between the macros via dynamic variables, or a
compile time context somehow?) would be more difficult to pull off /
not clear how to do it in Clojure for the author."

  (let [_nam (symbol (str "_" nam))]
    
    `(do (defmacro ~nam [& args#]
           ;; `~~_nam and `~'~_nam don't work, thus use org.clojure/tools.reader
           (cons* (syntax-quote ~_nam) '~'_tree-ctx args#))
         ~(if (seq binds&body)
              (let [[binds & body] binds&body]
                `(defn ~_nam ~(vector-cons '_tree-ctx binds)
                   ~@body))))))

(defmacro def* 
  "Companion for `defn*`, for cases where `nam` is to be defined from
an expression; it just creates the wrapper macro, and depends on the
expression returning a function that takes `_tree-ctx` as the first
argument."
  ([nam maybe-docstring expr]
   (let [_nam (symbol (str "_" nam))]
    
     `(do (defmacro ~nam [& args#]
            ~@(if maybe-docstring (list maybe-docstring) '())
            (cons* (syntax-quote ~_nam) '~'_tree-ctx args#))
          (def ~_nam
               ~@(if maybe-docstring (list maybe-docstring) '())
               ~expr))))
  ([nam expr]
   `(def* ~nam nil ~expr)))

