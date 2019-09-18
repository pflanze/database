(ns chj.util)

(defn class-predicate-for [class]
  (fn [v]
      (instance? class v)))

(defn error
  ([msg]
   (throw (new Exception msg)))
  ([msg val]
   (throw (new Exception (str msg ": " val)))))


(defmacro ->* [& forms]
  `(fn [v#]
       (-> v# ~@forms)))

(defmacro ->>* [& forms]
  `(fn [v#]
       (->> v# ~@forms)))


(defn for-each [proc coll]
  (loop [s (seq coll)]
        (when s
              (proc (first s))
              (recur (next s)))))

;; (for-each pr [3 4 5])
;; is the same as
;; (doseq [v [3 4 5]] (pr v))


(defn hash-map-map [fn hashmap]
  (into {} (map fn hashmap)))


(defmacro with-gensym [sym & body]
  `(let [~sym (gensym '~sym)]
     ~@body))

