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


(def keyword->string
     (->* str (subs 1)))

;; XX is there a better way?
(defn vector-map [f v]
  (apply vector (map f v)))


;; also see https://groups.google.com/forum/#!topic/clojure/7gCK7izwXBc
;;   and look into https://github.com/LonoCloud/synthread
(defn flip [f]
  (fn [a b]
      (f b a)))

