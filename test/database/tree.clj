(ns test.database.tree
    (:require [clojure.test :refer :all]
              [chj.test :refer [is* is=]]
              [chj.threading :refer [defn*]]
              [database.tree
               :refer
               [
                node* node-branch-count red black
                rb:depth rb:count rb:balance-old rb:balance rb:add
                rb:conj rb:contains? rb:keys rb:vals rb:ref rb:into seq->rb
                rb:rkeys rb:rvals rb:seq rb:rseq
                GET PUT GET-deeply]]
              [database.store
               :refer
               [
                open-store store= reference
                store-statistics store-statistics-reset!
                Database? ->Database database-dostore database-donotstore]]
              [chj.util :refer [map-entry]]
              [chj.debug :refer :all]))


(def this (->Database (open-store "db")
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


(defn* Database? random-node* [ncases kmin kmax force-black?]
  "Does not PUT the immediate layer (does the deeper ones)"
  ;; XX doesn't currently enforce red vs black rules!
  (if (< kmin kmax)
      (let [i (rand-int ncases) k (random-k kmin kmax)]
        (if (< i 2)
            (let [
                  a (random-node* 10 kmin k false)
                  b (random-node* 10 (inc k) kmax false)]
              (node* (if (and (= i 0) (not force-black?)) :red :black)
                     (PUT a)
                     (map-entry k (str k))
                     (PUT b)
                     (+ (node-branch-count a) 1 (node-branch-count b))))
            nil))
      nil))

(defn* Database? random-node)
(defn _random-node
  ([this ncases kmin kmax force-black?]
   (PUT (random-node* ncases kmin kmax force-black?)))
  ([this ncases kmin kmax]
   (random-node ncases kmin kmax false))
  ([this]
   (random-node 3 10 40 false)))

(defn* Database? random-node-other-than [seen kmin kmax force-black?]
  (loop []
        (let [n (random-node 3 kmin kmax force-black?)]
          (if (contains? seen n)
              (recur)
              n))))


(defn* Database? test-balance [n]
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
  (or (t-bal node)
      (= (p "NEW" (GET (rb:balance node))) (p "OLD" (GET (rb:balance-old node))))))


(defn test-balance-old [n]
  ;; show that t-balance and t-balance-old behave the same way for
  ;; in-memory trees
  (dotimes [rep n]
           (let [node (_random-node (database-donotstore))]
             (is (t-bal-p node)))))

(deftest t-balance-old
  (test-balance-old 100))


(defn test-balance-save [n]
  ;; show that t-balance behaves the same for in-memory trees as for
  ;; saved ones.
  (dotimes [rep n]
           (let [node (_random-node (database-dostore))]
             (is (= (rb:balance (GET-deeply node))
                    (GET-deeply (rb:balance node)))))))

(deftest t-balance-save
  (test-balance-save 100))



(defn range-kvs [from to]
  (map #(vector % (str %))
       (range from to)))

(deftest skewed
  (let [t (fn [this]
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
    (t (database-donotstore))
    (t (database-dostore))

    ;; get a value via the cache:
    (def this2
         (let [this (->Database (open-store "db")
                                false)]
           (letfn [(make-reference
                    []
                    (let [this (:the-store this)]
                      (reference "RAg+A7UHrw0pHYa4aAMCPY71_9fixgHhOPw7NyA0J14")))
                   (tst [r]
                        (is= (GET r)
                             (black nil (map-entry 12 "12") nil)))]

                  (let [r1 (make-reference) r2 (make-reference)]

                    (is= (store-statistics (:the-store this))
                         [0 0])

                    (tst r1)
                    (is= (store-statistics (:the-store this))
                         [0 1])

                    (tst r1)
                    (is= (store-statistics (:the-store this))
                         ;; no store access since r1 caches value
                         ;; directly; xx weakref will make this
                         ;; unreliable
                         [0 1])

                    (tst r2)
                    (is= (store-statistics (:the-store this))
                         ;; cache hit
                         [1 1])

                    (def r3 (make-reference))
                    (is= (store-statistics (:the-store this))
                         ;; cache hit
                         [2 1])

                    (tst r3)
                    (is= (store-statistics (:the-store this))
                         ;; no change since r3 already caches
                         ;; value directly; xx weakref ditto
                         [2 1])))
           
           this))))



(defn random-rb []
  (let [nelt (rand-int 200)]
    (reduce (fn [t _]
                (let [i (rand-int 750)]
                  (rb:add t i (str i))))
            nil
            (range nelt))))

(defn test-count [nrep]
  (let [old @database.tree/node-count-count]
    (dotimes [rep nrep]
             (let [t (random-rb)]
               (is= (count (rb:seq t))
                    (rb:count t))))
    (- @database.tree/node-count-count old)))

(deftest t-count
  (test-count 20))


