(ns test.database.store
    (:require [clojure.test :refer :all]
              [database.store :as s]
              [chj.test :refer [is*]]))


(deftest unbroken-symbol-serialisation

  (def s (s/open-store "db"))

  (is*

   (= (->> (symbol "a b") (s/store-put s) (s/store-get s) type)
      clojure.lang.Symbol)

   (= (->> (symbol "a b") (s/store-put s) (s/store-get s))
      (symbol "a b"))))

