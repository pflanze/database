(ns test.database.dbrecord
    (:require [chj.test :refer [is*]])
    (:require [database.store :as s])
    (:require [database.dbrecord :refer [defdbrecord]]))


(defdbrecord pair [car cdr])

(is*
 
 (= (-> (Pair. 10 (Pair. 20 30)) s/put s/get)
    (Pair. 10 (Pair. 20 30)))

 (= (-> (pair 10 (pair 20 30)) s/put s/get)
    (pair 10 (pair 20 30))))

