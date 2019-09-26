(ns chj.util
    (:require [clojure.string :refer [join]]
              [clojure.core.match :refer [match]]))

(defn class-predicate-for [class]
  (fn [v]
      (instance? class v)))

(defn error
  ([msg]
   (throw (new Exception msg)))
  ([msg & vals]
   (throw (new Exception (apply str msg ": "
                                (join ", " (map pr-str vals)))))))


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


;; Error-checking hash-map handling: (TODO: extend to collections, maybe more)

(def no-value (gensym 'no-value))

(defn xref [m k]
  (let [v (m k no-value)]
    (if (identical? v no-value)
        ;; XX typed exceptions (oh, use Result ? but is explicit already)
        (error "key not found" k)
        v)))

(defn xconj [m k+v]
  (match k+v
         [k v]
         (let [v (m k no-value)]
           (if (identical? v no-value)
               (conj m k+v)
               (error "key already in map" k)))))


(defn xpartition [n s]
  "Like partition or partition-all but throws an exception when (count s) is not
evenly dividable by n"
  (if (zero? (rem (count s) n))
      (partition n s)
      (error "count(sequence) not even divisible by the partition size"
             (count s) n (last s))))


