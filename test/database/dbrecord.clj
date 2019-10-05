(ns test.database.dbrecord
    (:require [clojure.test :refer :all]
              [chj.test :refer [is* is=]]
              [database.store :as s]
              [database.dbrecord :refer [defdbrecord-expand defdbrecord]]))


'(deftest expansion
  (is= (defdbrecord-expand (atom {})
         'foo
         '[a kv b])
       '(do
            (clojure.core/defrecord Foo [a kv b])
            (def foo ->Foo)
          (clojure.core/defn
           foo?
           [v__6111__auto__]
           (clojure.core/instance? Foo v__6111__auto__))
          (database.store/add-transformer!
           (database.store/type-transformer
            Foo
            (quote foo)
            foo
            (clojure.core/fn
             [v6200]
             (clojure.core/list
              (quote foo)
              (database.store/type-transformer:to-code (:a v6200))
              (database.store/type-transformer:to-code (:kv v6200))
              (database.store/type-transformer:to-code (:b v6200)))))))))


(defdbrecord pair [car cdr])

(deftest constructor

  (is= (Pair. 10 (Pair. 20 30))
       (pair 10 (pair 20 30))))

(deftest reversibility

  (def this (s/open-store "db"))

  (is= (->> (pair 10 (pair 20 30)) s/store-put s/store-get)
       (pair 10 (pair 20 30))))

