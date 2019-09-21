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


