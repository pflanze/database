(ns
 ^{:author "London dojo group"
   :doc "Lowlevel storage"}
 database.store
 ;; (:require [multihash.core])
 )


(defrecord Store [path])


(defn store [st obj]
  (let [s (pr-str obj)]
    '(let [hash ()])))

(defn retrieve [st hash]
  'obj)
