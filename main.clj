(ns
 ^{:author "London dojo group"
   :doc "Main"}
 database.main
 (:require [database.database :as database]))


(def db (database/Database. ".tmp"))

