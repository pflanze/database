(ns chj.util)

(defn class-predicate-for [class]
  (fn [v]
      (instance? class v)))

