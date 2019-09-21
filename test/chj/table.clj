(ns test.chj.table
    (:require [clojure.test :refer :all]
              [chj.table :refer [entries->table table-ref table-add]]
              [chj.test :refer [is*]]))


(deftest basics
  (def t (entries->table [[1, "one"], [3, "three"]]))
  (def t2 (table-add t [4 "four"]))

  (is*
 
   (= (table-ref t2 second "four")
      [4 "four"])
   ;; (= (table-ref t2 second "four")
   ;;    [4 "four"])
   ;; ^ on second access, use cached table; need hooks for that.


   (= (table-ref t second "four")
      nil)
   ;; (= (table-ref t second "four")
   ;;    nil)
   ;; ditto


   (= (table-ref t first 1)
      [1 "one"])
   (= (table-ref t first 1)
      [1 "one"])
   ;; This one creates a fresh index:
   (= (table-ref t2 first 1)
      [1 "one"])
   (= (table-ref t2 first 1)
      [1 "one"])))

(deftest more-table-add?
  (def t3 (table-add t2 [5 "five"]))

  (is*
   (= (table-ref t3 first 1)
      [1 "one"])
   (= (table-ref t3 first 5)
      [5 "five"])))

