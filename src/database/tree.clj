;; License

;; Copyright © 2016 Henry Garner, 2019 Christian Jaeger & others (see
;; Git commit history)

;; Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [clojure.tools.reader :refer [syntax-quote]]
              [database.store :as s]
              [chj.debug :refer [p]]
              [chj.util :refer [flip vector-map ->vector vector-cons cons*
                                     => inc! either]]))


(defrecord TreeCtx [the-store store?])

(defn TreeCtx-store?-set [c b]
  (->TreeCtx (:the-store c)
             b))

(defn TreeCtx-donotstore [c] (TreeCtx-store?-set c false))
(defn TreeCtx-dostore [c] (TreeCtx-store?-set c true))


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



(defn* GET [x]
  (if (s/reference? x)
      (let [v (s/store-get (:the-store _tree-ctx) x)]
        (assert (not (s/reference? v)))
        v)
      x))



(defn redblack-keyword? [v]
  (case v
    (:red :black) true
    false))

(defn node? [v]
  (and (vector? v)
       (= (count v) 5)
       (redblack-keyword? (first v))))

(defn node-count [nod]
  (match nod
         nil 0
         [color a kv b count] count))

(def node-count-count (atom 0))

(def node-branch? (either nil? node? s/reference?))

(defn* node)
(defn _node
  "Make a new tree node"
  ([_tree-ctx color a kv b count]
   (=> redblack-keyword? color)
   (=> node-branch? a)
   (=> map-entry? kv)
   (=> node-branch? b)
   [color a kv b count])
  ([_tree-ctx color a kv b]
   (inc! node-count-count)
   (let [count (+ (node-count (GET a))
                  1
                  (node-count (GET b)))]
     (node color a kv b count))))

(defn node* [color]
  (fn ([_tree-ctx a kv b count] (node color a kv b count))
      ([_tree-ctx a kv b] (node color a kv b))))

(def* red (node* :red))
(def* black (node* :black))



(defn* GET-deeply [x]
  (let [x* (GET x)]
    (match x*
           [color a kv b cnt]
           (node color (GET-deeply a) kv (GET-deeply b) cnt)
           :else
           x*)))


(defn short-string? [v]
  (and (string? v)
       (<= (count v) 160)))

(defn not-needs-PUT? [v]
  (or (nil? v)
      (boolean? v)
      (number? v)
      (s/reference? v)
      (short-string? v)))

(defn* PUT [x]
  (if (=> boolean? (:store? _tree-ctx))
      (if (not-needs-PUT? x)
          x
          (s/store-put (:the-store _tree-ctx) x))
      x))

(defn* PUT-deeply [v]
  "This one is really specific to rb tree"
  (if (not-needs-PUT? v)
      v
      (match v
             [color a x b cnt]
             (PUT (if (and (not-needs-PUT? a)
                           (not-needs-PUT? b))
                      v
                      (node color
                            (PUT-deeply a)
                            x
                            (PUT-deeply b)
                            cnt)))

             :else
             (PUT v))))


(defn* rb:balance-old [tree]
  (match tree
         (:or [:black [:red [:red a x b _] y c _] z d _]
              [:black [:red a x [:red b y c _] _] z d _]
              [:black a x [:red [:red b y c _] z d _] _]
              [:black a x [:red b y [:red c z d _] _] _])
         (red (black a x b)
              y
              (black c z d))
         :else tree))

(defn* rb:balance [tree]
  (let [cont
        (fn [a b c d x y z]
            (red (black a x b)
                 y
                 (black c z d)))]
    (match (GET tree)
           [:black N1 z M1 _]
           (let [otherwise
                 (fn []
                     (match (GET M1)
                            [:red N2 z2 M2 _]
                            ;; [:black a  x [:red [:red b y c] z  d]]
                            ;; tree    N1 z M1    N2           z2 M2
                            (match (GET N2)
                                   [:red b y c _]
                                   ;;    a  b c d  x y z
                                   (cont N1 b c M2 z y z2)
                                   :else
                                   (match (GET M2)
                                          [:red c z3 d _]
                                          ;; [:black a  x [:red b  y  [:red c z  d]]]
                                          ;; tree    N1 z M1    N2 z2 M2    c z2 d
                                          (cont N1 N2 c d z z2 z3)
                                          :else
                                          tree))
                            :else
                            tree))]
             (match (GET N1)
                    [:red N2 y M2 _]
                    (match (GET N2)
                           ;; [:black [:red [:red a x b] y c] z d]
                           [:red a x b _]
                           ;;    a b c  d
                           (cont a b M2 M1 x y z)

                           :else
                           (match (GET M2)
                                  [:red b y2 c _]
                                  ;; [:black [:red a  x  [:red b y  c]] z d]
                                  ;; tree    N1    N2 y  M2    b y2 c   z M1
                                  ;;    a  b c d  x y  z
                                  (cont N2 b c M1 y y2 z)
                                  :else
                                  (otherwise)))
                    :else
                    (otherwise)))
           :else tree)))


(defn* rb:add [tree k v]
  (let [ins
        (fn ins [tree]
            (let [tree (GET tree)]
              (match tree

                     nil
                     (red nil (clojure.lang.MapEntry. k v) nil)

                     [color a kv b _]
                     (case (compare k (key kv))
                       -1 (rb:balance (node color (ins a) kv b))
                       1 (rb:balance (node color a kv (ins b)))
                       0 tree))))
        [_ a y b]
        (ins tree)]
    (PUT-deeply (black a y b))))

(defn* rb:conj [tree [k v]]
  (rb:add tree k v))



;; XX name, or how ?
(defn* rb:into [tree pairs]
  (reduce #(rb:conj %1 %2) tree pairs))

(defn* seq->rb [s]
  (rb:into nil s))


(defn* rb:contains?
  [tree k]
  ;; XX does this work as doc-string?
  "Check if the key is present"
  (loop [tree tree]
        (match (GET tree)
               nil false
               [_ a kv b _] (case (compare k (key kv))
                              -1 (recur a)
                              1 (recur b)
                              0 true))))


(defn* rb:ref)
(defn _rb:ref
  "Check if the key is present and return value or a default"
  ([_tree-ctx tree k not-found]
   (loop [tree tree k k]
         (match (GET tree)
                nil not-found
                [_ a kv b _] (case (compare k (key kv))
                               -1 (recur a k)
                               1 (recur b k)
                               0 (val kv)))))
  ([_tree-ctx tree k]
   (_rb:ref _tree-ctx tree k nil)))


(defn rb-reducer-lazy [cons access v0 direction]
  (fn [_tree-ctx tree]
      (letfn [(rec [tree tail]
                   (lazy-seq
                    (match (GET tree)
                           nil
                           tail
                           [_col a y b _]
                           (rec (direction a b)
                                (cons (access y)
                                      (rec (direction b a) tail))))))]
             (rec tree v0))))

;; copy-paste except for eliminated lazy-seq
(defn rb-reducer [cons access v0 direction]
  (fn [_tree-ctx tree]
      (letfn [(rec [tree tail]
                   (match (GET tree)
                          nil tail
                          [_col a y b _]
                          (rec (direction a b)
                               (cons (access y)
                                     (rec (direction b a) tail)))))]
             (rec tree v0))))

(defn forward-select [a b] a)
(defn backward-select [a b] b)

(def* rb:keys (rb-reducer-lazy cons key '() forward-select))
(def* rb:vals (rb-reducer-lazy cons val '() forward-select))

(def* rb:rkeys (rb-reducer-lazy cons key '() backward-select))
(def* rb:rvals (rb-reducer-lazy cons val '() backward-select))


(def* rb:seq (rb-reducer-lazy cons identity '() forward-select))
(def* rb:rseq (rb-reducer-lazy cons identity '() backward-select))


(defn* rb:depth [tree]
  "The max depth of the tree, for debugging purposes"
  (match (GET tree)
         nil
         0
         [_ a kv b _]
         (inc (max (rb:depth a)
                   (rb:depth b)))))

(defn* rb:count [tree]
  "The number of associations in the tree"
  (node-count (GET tree)))


;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
