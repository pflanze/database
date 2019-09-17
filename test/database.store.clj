(ns test.database.store
    (:require [database.store :as db])
    (:require [clojure.test :refer [is]]))


(is (->> (symbol "a b") db/put db/get type)
    clojure.lang.Symbol)

(is (->> (symbol "a b") db/put db/get)
    (symbol "a b"))

