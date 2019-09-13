(ns chj.util)

(defn class-predicate-for [class]
  (fn [v]
      (instance? class v)))

(defn error
  ([msg]
   (throw (new Exception msg)))
  ([msg val]
   (throw (new Exception (str msg ": " val)))))


