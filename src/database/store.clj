(ns database.store
    ;;(:require [clojure.core :exclude [get]]) nope,
    (:refer-clojure :exclude [get])
    (:require [chj.debug :refer [p pseq]])
    (:require [chj.util :refer [class-predicate-for
                                error
                                ->* ->>*]])
    (:import [java.io ByteArrayInputStream
                      ByteArrayOutputStream
                      ObjectOutputStream
                      ObjectInputStream
                      FileOutputStream
                      FileInputStream]
             [java.security MessageDigest]
             [java.math BigInteger]
             [java.util Base64]))


(defrecord Store [path])

(def the-store (Store. "db"))



(defn chop [s]
  (.substring s 0 (dec (.length s))))

(defn rawhash->string [bytes]
  (-> (.encodeToString (Base64/getEncoder) bytes)
      (.replace \/ \_)
      (chop)))

;; (defn string->rawhash [s]
;;   (.decode (Base64/getDecoder) (-> s
;;                                    (.replace \_ \/)
;;                                    (str \=))))


(def hash-algorithm
     (MessageDigest/getInstance "SHA-256"))

(defn our-hash [str]
  (rawhash->string (.digest hash-algorithm (.getBytes str))))


(defrecord Reference [hash])


(defn hash-path [hash]
  (str (:path the-store) "/" hash))

(defn reference-path [ref]
  (hash-path (:hash ref)))

;; Constructor for serialisation

(defn reference [str]
  (Reference. str))


(defrecord TypeTransformer [type constructorname constructor to-code])

(def TypeTransformer? (class-predicate-for TypeTransformer))

(defn identityTransformer [typ]
  (TypeTransformer. typ
                    false
                    false
                    identity))

(declare serialize:to-code)

(defn serialization-not-supported [v]
  (error "serialization of this type is not supported" (type v)))

(def type-transformers
     [(TypeTransformer. clojure.lang.LongRange
                        false ;; 'range
                        false ;; range
                        ;; (fn [v]
                        ;;     (list 'range
                        ;;           (.first v)
                        ;; Oops, no accessors for those:
                        ;;           (.end v)
                        ;;           (.step v)))
                        serialization-not-supported)
      (TypeTransformer. clojure.lang.LazySeq
                        false ;; 'LazySeq
                        false
                        serialization-not-supported)
      (TypeTransformer. clojure.lang.PersistentList
                        'list
                        list
                        (fn [v]
                            (cons 'list (map serialize:to-code v))))
      (TypeTransformer. clojure.lang.Symbol
                        'symbol
                        symbol
                        (fn [v]
                            (list 'symbol (str v))))
      (TypeTransformer. database.store.Reference
                        'reference
                        reference
                        (fn [v]
                            (list 'reference (:hash v))))
      (TypeTransformer. clojure.lang.PersistentVector
                        'vector
                        vector
                        (fn [v]
                            (cons 'vector (map serialize:to-code v))))
      (identityTransformer java.lang.Long)
      (identityTransformer java.lang.String)
      (identityTransformer java.lang.Boolean)
      (identityTransformer clojure.lang.Ratio)
      (identityTransformer java.lang.Double)
      (identityTransformer nil)])


(defn index
  ([tbl key]
   (index tbl key identity))
  ([tbl key val]
   (apply hash-map
          (reduce (fn [res d]
                      (cons (key d)
                            (cons (val d)
                                  res)))
                  '()
                  type-transformers))))

(def type->to-code)
(def constructorname->constructor)

(defn index-transformers! []
  (def type->to-code
       (index type-transformers :type :to-code))
  (def constructorname->constructor
       (index type-transformers :constructorname :constructor)))

(index-transformers!)

(defn add-transformer! [^TypeTransformer t]
  (assert (TypeTransformer? t))
  (def type-transformers (conj type-transformers t))
  (index-transformers!))



(defn serialize:to-code [obj]
  (if-let [ser (type->to-code (type obj))]
          (ser obj)
          (error "serialize: don't know about this type" (type obj))))

(defn serialize [obj]
  (pr-str (serialize:to-code obj)))

(defn deserialize:eval [e]
  (cond (list? e)
        (apply (constructorname->constructor (first e))
               (map deserialize:eval (rest e)))
        true
        e))

(def deserialize-stream
     (->* java.io.InputStreamReader.
          java.io.PushbackReader.
          read
          deserialize:eval))

(def deserialize-string
     (->* .getBytes ByteArrayInputStream. deserialize-stream))

(defn deserialize-file [path]
  (with-open [in (FileInputStream. path)]
             (deserialize-stream in)))


(defn put [obj]
  (let [s
        (serialize obj)
        hash
        (our-hash s)
        path
        (hash-path hash)]
    (spit path s)
    (Reference. hash)))

(defn get [ref]
  (-> (reference-path ref)
      (deserialize-file)))




