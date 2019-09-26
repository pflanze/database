(ns test.chj.result
    (:require [chj.result :refer [->Ok ->Err match-Result if-Ok]]
              [chj.test :refer [is*]]
              [clojure.test :refer :all]
              [clojure.core.match :refer [match]]))


(deftest t-match-Result
  (is*
   (= (match-Result (->Ok 10)
                    (Ok a) a)
      10)

   (thrown-with-msg? Exception #"no match for:"
                     (match-Result (->Err 10)
                                   (Ok a) a))

   (defn t [v]
     (match-Result v
                   (Ok a) [a :ok]
                   (Err b) [b :err]))
   
   (= (t (->Ok 10))
      [10 :ok])

   (= (t (->Err 11))
      [11 :err])))


(deftest t-if-Ok
  (is*
   (= (if-Ok (->Ok 10) it)
      10)
   (= (if-Ok (->Ok 11)
             it
             :nope)
      11)
   (= (if-Ok (->Err 12)
             :not
             it)
      12)))
