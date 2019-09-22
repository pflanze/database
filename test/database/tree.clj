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


