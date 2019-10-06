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


(defn predicate-error [pred val]
  (error "predicate does not accept value" pred val))

(defmacro => [pred expr]
  `(let [p# ~pred v# ~expr]
     (if (p# v#)
         v#
         (predicate-error '~pred v#))))


(defmacro ->* [& forms]
  `(fn [v#]
       (-> v# ~@forms)))

(defmacro ->>* [& forms]
  `(fn [v#]
       (->> v# ~@forms)))


(defn for-each [proc coll]
  (doseq [v coll]
         (proc v)))


(defn hash-map-map [fn hashmap]
  (into {} (map fn hashmap)))


(defmacro with-gensym [sym & body]
  `(let [~sym (gensym '~sym)]
     ~@body))


(defn keyword->string [v]
  (=> keyword? v)
  (-> v str (subs 1)))

(defn symbol->string [v]
  (=> symbol? v)
  ;; Is this always correct?
  (str v))

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

(defn xconj [c kv]
  (cond
   (map? c)
   (match kv
          [k v]
          (if (contains? c k)
              (error "key already in map" k)
              (conj c kv)))

   (coll? c)
   (if (contains? c kv)
       (error "value already in collection" kv)
       (conj c kv))

   :else
   (error "xconj: no method for" (type c))))


(defn xpartition [n s]
  "Like partition or partition-all but throws an exception when (count s) is not
evenly dividable by n"
  (if (zero? (rem (count s) n))
      (partition n s)
      (error "(count sequence) not even divisible by the partition size"
             (count s) n (last s))))



;;XX forgot what Clojure names this
(defn cons* [a b r]
  (cons a (cons b r)))

(defn ->vector [s]
  (apply vector s))

(defn vector-cons [a s]
  (->vector (cons a s)))


(defn inc! [a]
  (swap! a inc))

(defn dec! [a]
  (swap! a inc))


(defmacro either [& cases]
  (with-gensym
   V
   `(fn [~V]
        (or ~@(map (fn [case]
                       `(~case ~V))
                   cases)))))

(defmacro maybe [pred]
  `(either nil? ~pred))

(defn type? [v]
  (or (= (type v) java.lang.Class)
      ;; (type nil) is nil, so nil is valid as a type, sigh:
      (nil? v)))


(defn map-entry [k v]
  (clojure.lang.MapEntry. k v))

;; map-entry? is in clojure.core


(defn make-comparison [<]
  (fn
   ([a b] (< (compare a b) 0))
   ([a b c] (and (< (compare a b) 0)
                 (< (compare b c) 0)))))

(def compare< (make-comparison <))
(def compare<= (make-comparison <=))
(def compare> (make-comparison >))
(def compare>= (make-comparison >=))
(def compare= (make-comparison =))

(defn char-alpha-numeric? [c]
  (or (compare<= \a c \z)
      (compare<= \A c \Z)
      (compare<= \0 c \9)
      (compare= c \_)))

(defn char-symbol-safe? [c]
  (or (char-alpha-numeric? c)
      (compare= c \-)
      (compare= c \+)
      (compare= c \*)
      (compare= c \%)
      ;; & isn't safe if by itself? ah it is
      (compare= c \&)
      (compare= c \?)
      (compare= c \!)))

(defn alpha-numeric? [s]
  (every? char-alpha-numeric? s))

(defn symbol-safe? [s]
  (if-let [s (seq s)]
          (every? char-symbol-safe? s)))


;; (def long? (class-predicate-for Long))
;; Even though (type 123) is java.lang.Long, int? returns true for it

(defn natural0-int? [n]
  (and (int? n)
       (not (neg? n))))

(defn long-bitnumber? [n]
  (and (int? n)
       (<= 0 n 63)))

(defn exp2 [n]
  (=> natural0-int? n)
  (assert (<= n 62))
  (bit-shift-left 1 n))

