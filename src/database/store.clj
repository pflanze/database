(ns
 ^{:author "London dojo group"
   :doc "Lowlevel storage"}
 database.store
 ;; (:require [multihash.core])
 (:require [clojure.pprint])
 )


(defrecord Store [path])


(defn store [st obj]
  (let [s (with-out-str (clojure.pprint/write obj))]
    '(let [hash ()])))

(defn retrieve [st hash]
  'obj)
