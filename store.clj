(ns
 ^{:author "London dojo group"
   :doc "Lowlevel storage"}
 database.store
 (:require [database.database :as database]))


(defn store [db obj]
  (let [s (with-out-str (clojure.pprint/write obj))]
    '(let [hash ()])))

(defn retrieve [db hash]
  'obj)
