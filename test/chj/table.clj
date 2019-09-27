(ns test.chj.table
    (:require [clojure.test :refer :all]
              [chj.table :refer [entries->table
                                 table-ref
                                 table-add
                                 table-indices
                                 table-indices-keyset]]
              [chj.test :refer [is* is=]]))


(deftest basics+indexing
  (def t (entries->table [[1, "one"], [3, "three"]]))
  (def t2 (table-add t [4 "four"]))

  ;; to verify re-use of indices
  (def ti0 (table-indices t))
  (def t2i0 (table-indices t2))
  (is (not (identical? ti0 t2i0))) ;; not relevant, both empty
  (is (= ti0 t2i0))
  

  (is= (table-ref t2 second "four")
       [4 "four"])

  (def ti1 (table-indices t))
  (def t2i1 (table-indices t2))
  (is (identical? ti0 ti1))
  (is (not (identical? t2i0 t2i1)))
  (is (not (= t2i0 t2i1)))
  
  ;; on second access, use cached table:
  (is= (table-ref t2 second "four")
       [4 "four"])

  (def ti2 (table-indices t))
  (def t2i2 (table-indices t2))
  (is (identical? ti1 ti2))
  (is (identical? t2i1 t2i2))


  (is= (table-ref t second "four")
       nil)

  (def ti3 (table-indices t))
  (def t2i3 (table-indices t2))
  (is (not (identical? ti2 ti3)))
  (is (identical? t2i2 t2i3))

  (is= (table-ref t second "four")
       nil)

  (def ti4 (table-indices t))
  (def t2i4 (table-indices t2))
  (is (identical? ti3 ti4))
  (is (identical? t2i3 t2i4))
  (is (= (table-indices-keyset t)
         #{second}))

  (is= (table-ref t first 1)
       [1 "one"])

  (is (= (table-indices-keyset t)
         #{first second}))
  
  ;; This one creates a fresh index:
  (is= (table-ref t2 first 1)
       [1 "one"])
  (is= (table-ref t2 first 1)
       [1 "one"])

  (is (= (table-indices-keyset t)
         #{first second}))
  (is (not (= (table-indices t)
              (table-indices t2))))


  (def t3 (table-add t2 [5 "five"]))

  (is*

   (thrown-with-msg?
    Exception
    #"unique index already contains an entry with this field, value:.*first"
    (table-add t3 [5 "five 2"]))

   (thrown-with-msg?
    Exception
    #"unique index already contains an entry with this field, value:.*second"
    (table-add t3 [6 "five"]))

   (= (table-ref t3 first 1)
      [1 "one"])
   (= (table-ref t3 first 5)
      [5 "five"])))


