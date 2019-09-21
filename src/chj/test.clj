(ns chj.test
    (:require [clojure.test :refer [is]]))


(defmacro is* [& forms]
  `(do ~@(map (fn [form]
                  `(is ~form))
              forms)))


(defmacro is= [a b]
  `(is (= ~a ~b)))

