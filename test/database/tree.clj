(ns test.database.tree
    (:require [clojure.test :refer :all]
              [chj.test :refer [is* is=]]
              [chj.threading :refer [defn*]]
              [database.tree
               :refer
               [
                node red black
                rb:depth rb:count rb:balance-old rb:balance rb:add
                rb:conj rb:contains? rb:keys rb:vals rb:ref rb:into seq->rb
                rb:rkeys rb:rvals rb:seq rb:rseq
                ->TreeCtx TreeCtx-dostore TreeCtx-donotstore
                GET PUT GET-deeply]]
              [database.store
               :refer
               [
                open-store store= reference
                           store-statistics store-statistics-reset!]]
              [chj.util :refer [map-entry]]
              [chj.debug :refer :all]))


(def _* (->TreeCtx (open-store "db")
                          false))

(deftest basics
  
  (def t nil)
  (def t (rb:add t 10 "ten"))
  (def t (rb:add t 20 "twenty"))

  (is= (rb:ref t 10)
       "ten")
  (is= (rb:ref t 20)
       "twenty")  
  (is= (rb:ref t 30)
       nil)
  (is= (rb:ref t 30 :n)
       :n)

  (is= (map #(rb:contains? t %)
            '(10 20 30))
       '(true true false))

  (is= (rb:keys t)
       '(10 20))
  (is= (rb:vals t)
       '("ten" "twenty")))


(deftest t-seq->rb+depth

  (def s [[10 "ten"] [20 "twenty"]])

  (def t1 (seq->rb s))
  (def t2 (seq->rb (reverse s)))
  
  (is*
   (= t1
      [:black nil [10 "ten"] [:red nil [20 "twenty"] nil 1] 2])
   (= (rb:depth t1)
      2)
   (= t2
      [:black [:red nil [10 "ten"] nil 1] [20 "twenty"] nil 2])
   (= (rb:depth t2)
      2)
   (= (rb:depth nil)
      0)
   (= (rb:depth (rb:add nil 4 "4"))
      1)))


(defn random-k [kmin kmax]
  "random key in given range, excl. kmax"
  (+ kmin (rand-int (- kmax kmin))))

(defn* random-node)
(defn _random-node
  ;; XX doesn't currently enforce red vs black rules!
  ([_* ncases kmin kmax force-black?]
   (if (< kmin kmax)
       (let [i (rand-int ncases) k (random-k kmin kmax)]
         (if (< i 2)
             (PUT (node
                   (if (and (= i 0) (not force-black?))
                       :red
                       :black)
                   (random-node 10 kmin k)
                   (map-entry k (str k))
                   (random-node 10 (inc k) kmax)))
             nil))
       nil))
  ([_* ncases kmin kmax]
   (random-node ncases kmin kmax false))
  ([_* ]
   (random-node 3 10 40 false)))

(defn* random-node-other-than [seen kmin kmax force-black?]
  (loop []
        (let [n (random-node 3 kmin kmax force-black?)]
          (if (contains? seen n)
              (recur)
              n))))


(defn* test-balance [n]
  (dotimes [rep n]
           (let [
                 ;; a and b must be black (or nil) to avoid
                 ;; accidentally matching anyway which would associate
                 ;; them with the wrong balanced positions in the
                 ;; test.
                 a (random-node-other-than #{} 1 10 true)
                 x (map-entry 10 "ten")
                 b (random-node-other-than #{a} 11 20 true)
                 y (map-entry 20 "twenty")
                 c (random-node-other-than #{a b} 21 30 false)
                 z (map-entry 30 "thirty")
                 d (random-node-other-than #{a b c} 31 40 false)

                 balanced (red (black a x b)
                               y
                               (black c z d))]
             
             (is*
              (= (rb:balance (black (red (red a x b) y c) z d)) balanced)
              (= (rb:balance (black (red a x (red b y c)) z d)) balanced)
              (= (rb:balance (black a x (red (red b y c) z d))) balanced)
              (= (rb:balance (black a x (red b y (red c z d)))) balanced)))))

(deftest t-balance
  (test-balance 100))



(defn t-bal [node]
  (= (GET (rb:balance node)) (GET (rb:balance-old node))))

(defn t-bal-p [node]
  (= (p "NEW" (GET (rb:balance node))) (p "OLD" (GET (rb:balance-old node)))))


(defn test-balance-old [n]
  ;; show that t-balance and t-balance-old behave the same way for
  ;; in-memory trees
  (dotimes [rep n]
           (let [node (_random-node (TreeCtx-donotstore _*))]
             (is (t-bal node)))))

(deftest t-balance-old
  (test-balance-old 100))


(defn test-balance-save [n]
  ;; show that t-balance behaves the same for in-memory trees as for
  ;; saved ones.
  (dotimes [rep n]
           (let [node (_random-node (TreeCtx-dostore _*))]
             (is (= (rb:balance (GET-deeply node))
                    (GET-deeply (rb:balance node)))))))

(deftest t-balance-save
  (test-balance-save 100))



(defn range-kvs [from to]
  (map #(vector % (str %))
       (range from to)))

(deftest skewed
  (let [t (fn [_*]
              (def t3 (seq->rb (range-kvs 10 20)))
              (is= (rb:depth t3)
                   5)
              (is= (rb:count t3)
                   10)

              (def t4 (rb:into t3 (reverse (range-kvs 40 50))))
              (is= (rb:depth t4)
                   6)
              (is= (rb:count t4)
                   20)

              (def t5 (rb:into t4 (range-kvs 40 500)))
              (is= (rb:depth t5)
                   12)
              (is= (rb:count t5)
                   470)

              (def t5b (rb:into t5 (range-kvs 40 500)))
              (is= (rb:depth t5)
                   12)
              (is= (rb:count t5b)
                   470)

              (def t5c (rb:into t5 (reverse (range-kvs 40 500))))
              (is (store= t5 t5c))

              (is= (take 5 (rb:keys t5))
                   '(10 11 12 13 14))
              (is= (take 5 (rb:rkeys t5))
                   '(499 498 497 496 495))

              (is= (take 2 (rb:vals t5))
                   '("10" "11"))
              (is= (take 3 (rb:rvals t5))
                   '("499" "498" "497"))

              (is= (take 5 (rb:seq t5))
                   '([10 "10"] [11 "11"] [12 "12"] [13 "13"] [14 "14"]))
              (is= (take 3 (rb:rseq t5))
                   '([499 "499"] [498 "498"] [497 "497"]))
              (is= (take 3 (keys (rb:rseq t5)))
                   '(499 498 497))

              (is= (last (rb:seq t5))
                   [499 "499"])
              
              )]
    (t (TreeCtx-donotstore _*))
    (t (TreeCtx-dostore _*))

    ;; get a value via the cache:
    (def _tree-ctx2
         (let [_* (->TreeCtx (open-store "db")
                                    false)]
           (is= (store-statistics (:the-store _*))
                [0 0])
           (is= (GET (reference "4h2jpHsL_nNsOoIYQWeVwsFd509R0bOK2P+6TPmST3g"))
                [:black nil [12 "12"] nil])
           (is= (store-statistics (:the-store _*))
                [0 1])
           (is= (GET (reference "4h2jpHsL_nNsOoIYQWeVwsFd509R0bOK2P+6TPmST3g"))
                [:black nil [12 "12"] nil])
           (is= (store-statistics (:the-store _*))
                [1 1])
           _*))))


