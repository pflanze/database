;; License

;; Copyright © 2016 Henry Garner, 2019 Christian Jaeger & others (see
;; Git commit history)

;; Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [database.store :as s]
              [chj.threading :refer [defn* def*]]
              [chj.debug :refer [p]]
              [chj.util :refer [=> inc! either error ->vector]]
              [database.dbrecord :refer [defdbrecord]]))



(defn* s/Database? GET [x]
  (if (s/reference? x)
      (let [v (s/_store-get (:the-store this) x)]
        (assert (not (s/reference? v)))
        v)
      x))



(defdbrecord Node [color a kv b count])
(def node? Node?)

(defn redblack-keyword? [v]
  (case v
    (:red :black) true
    false))


(defn node-branch-count [nod]
  (if (nil? nod)
      0
      (:count nod)))

(def node-count-count (atom 0))

(def node-branch? (either nil? node? s/reference?))

(defn node [color a kv b count]
  "The full node constructor, no need to calculate count (and hence no
need to force a or b into memory"
  (=> redblack-keyword? color)
  (=> node-branch? a)
  (=> map-entry? kv)
  (=> node-branch? b)
  (->Node color a kv b count))

(defn* s/Database? node* [color a kv b]
  "Make a new tree node"
  (inc! node-count-count)
  (let [count (+ (node-branch-count (GET a))
                 1
                 (node-branch-count (GET b)))]
    (node color a kv b count)))

(defn node_color* [color]
  (fn [this a kv b] (node* color a kv b)))

(def* s/Database? red* (node_color* :red))
(def* s/Database? black* (node_color* :black))

(defn node_color [color]
  (fn [a kv b count] (node color a kv b count)))

(def red (node_color :red))
(def black (node_color :black))


(defn error-not-a-node [nod]
  (error "not a node*" nod))


(defn node-color [nod] (:color nod))
(defn node-a [nod] (:a nod))
(defn node-kv [nod] (:kv nod))
(defn node-b [nod] (:b nod))
(defn node-count [nod] (:count nod))

(defn node->black [nod]
  (black (:a nod)
          (:kv nod)
          (:b nod)
          (:count nod)))


;; Predicates on node-branch
(defn red?* [nod]
  (and nod
       (case (:color nod)
         :red true
         :black false
         (error-not-a-node nod))))

(defn black?* [nod]
  (and nod
       (case (:color nod)
         :red false
         :black true
         (error-not-a-node nod))))

;; Meh-replacement for match:
(defmacro with-node [nod & body]
  `(let [
         ~'color (node-color ~nod)
         ~'a (node-a ~nod)
         ~'kv (node-kv ~nod)
         ~'b (node-b ~nod)
         ~'count (node-count ~nod)
         ]
     ~@body))



(defn* s/Database? GET-deeply [x]
  (let [x' (GET x)]
    (if (node? x')
        (node (node-color x')
               (GET-deeply (node-a x'))
               (node-kv x')
               (GET-deeply (node-b x'))
               (node-count x'))
        x')))


(defn short-string? [v]
  (and (string? v)
       (<= (count v) 160)))

(defn not-needs-PUT? [v]
  (or (nil? v)
      (boolean? v)
      (number? v)
      (s/reference? v)
      (short-string? v)))

(defn* s/Database? PUT [x]
  (if (=> boolean? (:store? this))
      (if (not-needs-PUT? x)
          x
          (s/_store-put (:the-store this) x))
      x))

(defn* s/Database? PUT-deeply [v]
  "This one is really specific to rb tree"
  (if (not-needs-PUT? v)
      v
      (if (node? v)
          (let [a (node-a v) b (node-b v)]
            (PUT (if (and (not-needs-PUT? a)
                          (not-needs-PUT? b))
                     v
                     (node (node-color v)
                            (PUT-deeply a)
                            (node-kv v)
                            (PUT-deeply b)
                            (node-count v)))))

          (PUT v))))


(defn* s/Database? rb:balance-old [tree]
  (match (->vector (seq tree))
         (:or [:black [:red [:red a x b _] y c _] z d _]
              [:black [:red a x [:red b y c _] _] z d _]
              [:black a x [:red [:red b y c _] z d _] _]
              [:black a x [:red b y [:red c z d _] _] _])
         (red* (black* a x b)
              y
              (black* c z d))
         :else tree))

(defn* s/Database? rb:balance [tree]
  (let [cont
        (fn [a b c d x y z]
            (red* (black* a x b)
                 y
                 (black* c z d)))]
    (let [
          tree' (GET tree)
          N1 (node-a tree')
          z (node-kv tree')
          M1 (node-b tree')
          treecnt (node-count tree')
          ]
      
      (if (black?* tree')
          (let [
                N1'
                (GET N1)
                otherwise
                (fn [N1cnt]
                    (let [M1' (GET M1)]
                      (if (red?* M1')
                          ;; [:black a  x [:red [:red b y c] z  d]]
                          ;; tree    N1 z M1    N2           z2 M2
                          (let [
                                N2 (node-a M1')
                                z2 (node-kv M1')
                                M2 (node-b M1')
                                M1cnt (node-count M1')

                                N2' (GET N2)]
                            (if (red?* N2')
                                ;;    a  b c d  x y z
                                ;;(cont N1 b c M2 z y z2)
                                ;; Matched:
                                ;; (black* N1
                                ;;        z
                                ;;        (red* (red* b y c N2cnt)
                                ;;             z2
                                ;;             M2
                                ;;             M1cnt)
                                ;;        treecnt)
                                (let [
                                      b (node-a N2')
                                      y (node-kv N2')
                                      c (node-b N2')
                                      N2cnt (node-count N2')
                                
                                      bcnt (node-branch-count (GET b))
                                      N1zbcnt (+ N1cnt 1 bcnt)
                                      ]
                                  (red (black N1 z b N1zbcnt)
                                        y
                                        (black c z2 M2 (- treecnt N1zbcnt 1))
                                        treecnt))
                                   

                                (let [M2' (GET M2)]
                                  (if (red?* M2')
                                      ;; [:black a  x [:red b  y  [:red c z  d]]]
                                      ;; tree    N1 z M1    N2 z2 M2    c z2 d
                                      ;;(cont N1 N2 c d z z2 z3)
                                      (let [
                                            c (node-a M2')
                                            z3 (node-kv M2')
                                            d (node-b M2')
                                            
                                            N2cnt (node-branch-count N2')
                                            N1N2cnt (+ N1cnt N2cnt 1)]
                                        (red (black N1 z N2 N1N2cnt)
                                              z2
                                              (black c z3 d (- treecnt N1N2cnt 1))
                                              treecnt))

                                      tree))))
                          tree)))]
            (if (red?* N1')
                (let [
                      N2 (node-a N1')
                      y (node-kv N1')
                      M2 (node-b N1')
                      N1cnt (node-count N1')

                      N2' (GET N2)]

                  (if (red?* N2')
                      ;; [:black [:red [:red a x b] y c] z d]
                      (let [
                            a (node-a N2')
                            x (node-kv N2')
                            b (node-b N2')
                            N2cnt (node-count N2')]
                        ;;    a b c  d
                        ;;(cont a b M2 M1 x y z)
                        (red (black a x b N2cnt)
                              y
                              (black M2 z M1 (- treecnt N2cnt 1))
                              treecnt))

                      (let [M2' (GET M2)]
                        (if (red?* M2')
                            (let [
                                  b (node-a M2')
                                  y2 (node-kv M2')
                                  c (node-b M2')
                                  M2cnt (node-count M2')]
                              ;; [:black [:red a  x  [:red b y  c]] z d]
                              ;; tree    N1    N2 y  M2    b y2 c   z M1
                              ;;    a  b c d  x y  z
                              ;;(cont N2 b c M1 y y2 z)
                              ;; Matched:
                              ;; (black* (red* N2 y M2 N1cnt)
                              ;;        ;;   N2cnt M2cnt
                              ;;        z
                              ;;        M1 ;; M1cnt
                              ;;        treecnt)
                              (let [
                                    N2cnt (node-branch-count N2')
                                    M1cnt (- treecnt N1cnt 1)
                                    bcnt (node-branch-count (GET b))
                                    ccnt (- M2cnt bcnt 1)
                                    ]
                                (red (black N2 y b (+ N2cnt bcnt 1))
                                      y2
                                      (black c z M1 (+ ccnt M1cnt 1))
                                      treecnt)))

                            (otherwise N1cnt)))))
                
                (otherwise (node-branch-count N1'))))

          tree))))


(defn* s/Database? rb:add [tree k v]
  (let [ins
        (fn ins [tree]
            (if tree

                (with-node
                 tree
                 (case (compare k (key kv))
                   -1
                   (let [
                         n' (GET a)
                         n* (ins n')
                         count* (+ count (- (node-branch-count n*)
                                            (node-branch-count n')))]
                     ;; xx why is count* not simply (inc count) ?
                     (rb:balance (node color n* kv b count*)))
                   1
                   (let [
                         n' (GET b)
                         n* (ins n')
                         count* (+ count (- (node-branch-count n*)
                                            (node-branch-count n')))]
                     (rb:balance (node color a kv n* count*)))
                   0
                   tree))

                (red nil (clojure.lang.MapEntry. k v) nil 1)))
        tree*
        (ins (GET tree))]
    (PUT-deeply (node->black tree*))))

(defn* s/Database? rb:conj [tree [k v]]
  (rb:add tree k v))



;; XX name, or how ?
(defn* s/Database? rb:into [tree pairs]
  (reduce #(rb:conj %1 %2) tree pairs))

(defn* s/Database? seq->rb [s]
  (rb:into nil s))


(defn* s/Database? rb:contains?
  [tree k]
  ;; XX does this work as doc-string?
  "Check if the key is present"
  (loop [tree tree]
        (let [tree' (GET tree)]
          (if tree'
              (case (compare k (key (node-kv tree')))
                -1 (recur (node-a tree'))
                1 (recur (node-b tree'))
                0 true)
              false))))


(defn* s/Database? rb:ref)
(defn _rb:ref
  "Check if the key is present and return value or a default"
  ([this tree k not-found]
   (loop [tree tree k k]
         (let [tree' (GET tree)]
           (if tree'
               (case (compare k (key (node-kv tree')))
                 -1 (recur (node-a tree') k)
                 1 (recur (node-b tree') k)
                 0 (val (node-kv tree')))
               not-found))))
  ([this tree k]
   (_rb:ref this tree k nil)))


(defn rb-reducer-lazy [cons access v0 direction]
  (fn [this tree]
      (letfn [(rec [tree tail]
                   (lazy-seq
                    (let [tree' (GET tree)]
                      (if tree'
                          (let [a (node-a tree') b (node-b tree')]
                            (rec (direction a b)
                                 (cons (access (node-kv tree'))
                                       (rec (direction b a) tail))))
                          tail))))]
             (rec tree v0))))

;; copy-paste except for eliminated lazy-seq
(defn rb-reducer [cons access v0 direction]
  (fn [this tree]
      (letfn [(rec [tree tail]
                   (let [tree' (GET tree)]
                     (if tree'
                         (let [a (node-a tree') b (node-b tree')]
                           (rec (direction a b)
                                (cons (access (node-kv tree'))
                                      (rec (direction b a) tail))))
                         tail)))]
             (rec tree v0))))

(defn forward-select [a b] a)
(defn backward-select [a b] b)

(def* s/Database? rb:keys (rb-reducer-lazy cons key '() forward-select))
(def* s/Database? rb:vals (rb-reducer-lazy cons val '() forward-select))

(def* s/Database? rb:rkeys (rb-reducer-lazy cons key '() backward-select))
(def* s/Database? rb:rvals (rb-reducer-lazy cons val '() backward-select))


(def* s/Database? rb:seq (rb-reducer-lazy cons identity '() forward-select))
(def* s/Database? rb:rseq (rb-reducer-lazy cons identity '() backward-select))


(defn* s/Database? rb:depth [tree]
  "The max depth of the tree, for debugging purposes"
  (let [tree' (GET tree)]
    (if tree'
        (inc (max (rb:depth (node-a tree'))
                  (rb:depth (node-b tree'))))
        0)))

(defn* s/Database? rb:count [tree]
  "The number of associations in the tree"
  (node-branch-count (GET tree)))


;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
