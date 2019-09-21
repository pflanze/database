(ns test.database.store
    (:require [clojure.test :refer :all]
              [database.store :as db]
              [chj.test :refer [is*]]))


(deftest unbroken-symbol-serialisation
  (is*

   (= (->> (symbol "a b") db/put db/get type)
      clojure.lang.Symbol)

   (= (->> (symbol "a b") db/put db/get)
      (symbol "a b"))))

