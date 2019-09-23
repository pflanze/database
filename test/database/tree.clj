(ns test.database.tree
    (:require [clojure.test :refer :all]
              [chj.test :refer [is* is=]]
              [database.tree :refer :all]))


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


(deftest t-seq->rb

  (def s [[10 "ten"] [20 "twenty"]])

  (def t1 (seq->rb s))
  (def t2 (seq->rb (reverse s)))
  
  (is*
   (= t1
      [:black nil [10 "ten"] [:red nil [20 "twenty"] nil]])
   (= t2
      [:black [:red nil [10 "ten"] nil] [20 "twenty"] nil])))


(deftest t-depth
  (is*
   (= (rb:depth nil)
      0)
   (= (rb:depth (rb:add nil 4 "4"))
      1)
   (= t
      [:black nil [10 "ten"] [:red nil [20 "twenty"] nil]])
   (= (rb:depth t)
      2)
   (= t2
      [:black [:red nil [10 "ten"] nil] [20 "twenty"] nil])
   (= (rb:depth t2)
      2)))


(defn range-kvs [from to]
  (map #(vector % (str %))
       (range from to)))

(deftest skewed
  (let [t (fn []
              (def t3 (seq->rb (range-kvs 10 20)))
              (is= (rb:depth t3)
                   5)

              (def t4 (rb:into t3 (reverse (range-kvs 40 50))))
              (is= (rb:depth t4)
                   6)

              (def t5 (rb:into t4 (range-kvs 40 500)))
              (is= (rb:depth t5)
                   12)

              (def t5b (rb:into t5 (range-kvs 40 500)))
              (is= (rb:depth t5)
                   12)

              (def t5c (rb:into t5 (reverse (range-kvs 40 500))))
              (is= t5 t5c))]
    (t)
    (binding [*save?* true]
             (t))))
