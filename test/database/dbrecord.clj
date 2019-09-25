(ns test.database.dbrecord
    (:require [clojure.test :refer :all]
              [chj.test :refer [is*]]
              [database.store :as s]
              [database.dbrecord :refer [defdbrecord]]))


(defdbrecord pair [car cdr])

(deftest reversibility
  (is*
 
   (= (-> (Pair. 10 (Pair. 20 30)) s/store-put s/store-get)
      (Pair. 10 (Pair. 20 30)))

   (= (-> (pair 10 (pair 20 30)) s/store-put s/store-get)
      (pair 10 (pair 20 30)))))

