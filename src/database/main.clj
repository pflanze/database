(ns
 ^{:author "London dojo group"
   :doc "Main"}
 database.main
 (:require [database.store :as store]))


(def st (store/Store. ".tmp"))

