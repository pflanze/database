;; License

;; Copyright © 2016 Henry Garner, 2019 Christian Jaeger & others (see
;; Git commit history)

;; Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.

(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [database.store :as s]
              [chj.threading :refer [defn* def*]]
              [chj.debug :refer [p]]
              [chj.util :refer [=> inc! either]]))



(defn* s/Database? GET [x]
  (if (s/reference? x)
      (let [v (s/_store-get (:the-store this) x)]
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

(defn node* [color a kv b count]
  "The full node constructor, no need to calculate count (and hence no
need to force a or b into memory"
  (=> redblack-keyword? color)
  (=> node-branch? a)
  (=> map-entry? kv)
  (=> node-branch? b)
  [color a kv b count])

(defn* s/Database? node [color a kv b]
  "Make a new tree node"
  (inc! node-count-count)
  (let [count (+ (node-count (GET a))
                 1
                 (node-count (GET b)))]
    (node* color a kv b count)))

(defn node_color [color]
  (fn [this a kv b] (node color a kv b)))

(def* s/Database? red (node_color :red))
(def* s/Database? black (node_color :black))

(defn node_color* [color]
  (fn [a kv b count] (node* color a kv b count)))

(def red* (node_color* :red))
(def black* (node_color* :black))



(defn* s/Database? GET-deeply [x]
  (let [x* (GET x)]
    (match x*
           [color a kv b cnt]
           (node* color (GET-deeply a) kv (GET-deeply b) cnt)
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
      (match v
             [color a x b cnt]
             (PUT (if (and (not-needs-PUT? a)
                           (not-needs-PUT? b))
                      v
                      (node* color
                             (PUT-deeply a)
                             x
                             (PUT-deeply b)
                             cnt)))

             :else
             (PUT v))))


(defn* s/Database? rb:balance-old [tree]
  (match tree
         (:or [:black [:red [:red a x b _] y c _] z d _]
              [:black [:red a x [:red b y c _] _] z d _]
              [:black a x [:red [:red b y c _] z d _] _]
              [:black a x [:red b y [:red c z d _] _] _])
         (red (black a x b)
              y
              (black c z d))
         :else tree))

(defn* s/Database? rb:balance [tree]
  (let [cont
        (fn [a b c d x y z]
            (red (black a x b)
                 y
                 (black c z d)))]
    (match (GET tree)
           [:black N1 z M1 treecnt]
           (let [
                 N1'
                 (GET N1)
                 otherwise
                 (fn [N1cnt]
                     (match
                      (GET M1)
                      [:red N2 z2 M2 M1cnt]
                      ;; [:black a  x [:red [:red b y c] z  d]]
                      ;; tree    N1 z M1    N2           z2 M2
                      (let [N2' (GET N2)]
                        (match N2'
                               [:red b y c N2cnt]
                               ;;    a  b c d  x y z
                               ;;(cont N1 b c M2 z y z2)
                               ;; Matched:
                               ;; (black N1
                               ;;        z
                               ;;        (red (red b y c N2cnt)
                               ;;             z2
                               ;;             M2
                               ;;             M1cnt)
                               ;;        treecnt)
                               (let [
                                     bcnt (node-count (GET b))
                                     N1zbcnt (+ N1cnt 1 bcnt)
                                     ]
                                 (red* (black* N1 z b N1zbcnt)
                                       y
                                       (black* c z2 M2 (- treecnt N1zbcnt 1))
                                       treecnt))
                                   
                               :else
                               (match (GET M2)
                                      [:red c z3 d _]
                                      ;; [:black a  x [:red b  y  [:red c z  d]]]
                                      ;; tree    N1 z M1    N2 z2 M2    c z2 d
                                      ;;(cont N1 N2 c d z z2 z3)
                                      (let [
                                            N2cnt (node-count N2')
                                            N1N2cnt (+ N1cnt N2cnt 1)]
                                        (red* (black* N1 z N2 N1N2cnt)
                                              z2
                                              (black* c z3 d (- treecnt N1N2cnt 1))
                                              treecnt))

                                      :else
                                      tree)))
                      :else
                      tree))]
             (match N1'
                    [:red N2 y M2 N1cnt]
                    (let [N2' (GET N2)]
                      (match N2'
                             ;; [:black [:red [:red a x b] y c] z d]
                             [:red a x b N2cnt]
                             ;;    a b c  d
                             ;;(cont a b M2 M1 x y z)
                             (red* (black* a x b N2cnt)
                                   y
                                   (black* M2 z M1 (- treecnt N2cnt 1))
                                   treecnt)

                             :else
                             (match (GET M2)
                                    [:red b y2 c M2cnt]
                                    ;; [:black [:red a  x  [:red b y  c]] z d]
                                    ;; tree    N1    N2 y  M2    b y2 c   z M1
                                    ;;    a  b c d  x y  z
                                    ;;(cont N2 b c M1 y y2 z)
                                    ;; Matched:
                                    ;; (black (red N2 y M2 N1cnt)
                                    ;;        ;;   N2cnt M2cnt
                                    ;;        z
                                    ;;        M1 ;; M1cnt
                                    ;;        treecnt)
                                    (let [
                                          N2cnt (node-count N2')
                                          M1cnt (- treecnt N1cnt 1)
                                          bcnt (node-count (GET b))
                                          ccnt (- M2cnt bcnt 1)
                                          ]
                                      (red* (black* N2 y b (+ N2cnt bcnt 1))
                                            y2
                                            (black* c z M1 (+ ccnt M1cnt 1))
                                            treecnt))

                                    :else
                                    (otherwise N1cnt))))
                    :else
                    (otherwise (node-count N1'))))
           :else tree)))


(defn* s/Database? rb:add [tree k v]
  (let [ins
        (fn ins [tree]
            (match tree

                   nil
                   (red* nil (clojure.lang.MapEntry. k v) nil 1)

                   [color a kv b cnt]
                   (case (compare k (key kv))
                     -1
                     (let [
                           n' (GET a)
                           n* (ins n')
                           cnt* (+ cnt (- (node-count n*)
                                          (node-count n')))]
                       ;; xx why is cnt* not simply (inc cnt) ?
                       (rb:balance (node* color n* kv b cnt*)))
                     1
                     (let [
                           n' (GET b)
                           n* (ins n')
                           cnt* (+ cnt (- (node-count n*)
                                          (node-count n')))]
                       (rb:balance (node* color a kv n* cnt*)))
                     0
                     tree)))
        [_ a y b cnt]
        (ins (GET tree))]
    (PUT-deeply (black* a y b cnt))))

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
        (match (GET tree)
               nil false
               [_ a kv b _] (case (compare k (key kv))
                              -1 (recur a)
                              1 (recur b)
                              0 true))))


(defn* s/Database? rb:ref)
(defn _rb:ref
  "Check if the key is present and return value or a default"
  ([this tree k not-found]
   (loop [tree tree k k]
         (match (GET tree)
                nil not-found
                [_ a kv b _] (case (compare k (key kv))
                               -1 (recur a k)
                               1 (recur b k)
                               0 (val kv)))))
  ([this tree k]
   (_rb:ref this tree k nil)))


(defn rb-reducer-lazy [cons access v0 direction]
  (fn [this tree]
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
  (fn [this tree]
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

(def* s/Database? rb:keys (rb-reducer-lazy cons key '() forward-select))
(def* s/Database? rb:vals (rb-reducer-lazy cons val '() forward-select))

(def* s/Database? rb:rkeys (rb-reducer-lazy cons key '() backward-select))
(def* s/Database? rb:rvals (rb-reducer-lazy cons val '() backward-select))


(def* s/Database? rb:seq (rb-reducer-lazy cons identity '() forward-select))
(def* s/Database? rb:rseq (rb-reducer-lazy cons identity '() backward-select))


(defn* s/Database? rb:depth [tree]
  "The max depth of the tree, for debugging purposes"
  (match (GET tree)
         nil
         0
         [_ a kv b _]
         (inc (max (rb:depth a)
                   (rb:depth b)))))

(defn* s/Database? rb:count [tree]
  "The number of associations in the tree"
  (node-count (GET tree)))


;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
