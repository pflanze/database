(ns test.database.dbrecord
    (:require [clojure.test :refer :all]
              [chj.test :refer [is*]]
              [database.store :as s]
              [database.dbrecord :refer [defdbrecord]]))


(defdbrecord pair [car cdr])

(deftest reversibility

  (def s (s/open-store "db"))

  (is*
 
   (= (->> (Pair. 10 (Pair. 20 30)) (s/store-put s) (s/store-get s))
      (Pair. 10 (Pair. 20 30)))

   (= (->> (pair 10 (pair 20 30)) (s/store-put s) (s/store-get s))
      (pair 10 (pair 20 30)))))

