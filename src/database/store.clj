(ns database.store
    (:require [chj.debug :refer [p pseq]]
              [chj.io :refer [spit-frugally]]
              [chj.util :refer [class-predicate-for
                                for-each
                                error
                                ->* ->>*
                                keyword->string]]
              [chj.table :refer [entries->table table-add table-ref]])
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

(def reference? (class-predicate-for Reference))


(defn map-entry [k v]
  (clojure.lang.MapEntry. k v))

;; map-entry? is in clojure.core



(defrecord TypeTransformer [type constructorname constructor to-code])

(defn type-transformer [type constructorname constructor to-code]
  (TypeTransformer. type constructorname constructor to-code))

(def TypeTransformer? (class-predicate-for TypeTransformer))


(def type-transformers (atom (entries->table [])))

(defn add-transformer! [^TypeTransformer t]
  (assert (TypeTransformer? t))
  (swap! type-transformers
         (fn [ts]
             (table-add ts t))))

(defn add-transformers! [& ts]
  (for-each add-transformer! ts))


(defn identityTransformer [typ]
  (type-transformer typ
                    false
                    false
                    identity))

(declare type-transformer:to-code)

(defn serialization-not-supported [v]
  (error "serialization of this type is not supported" (type v)))

(add-transformers!
 (type-transformer clojure.lang.LongRange
                   false  ;; 'range
                   false  ;; range
                   ;; (fn [v]
                   ;;     (list 'range
                   ;;           (.first v)
                   ;; Oops, no accessors for those:
                   ;;           (.end v)
                   ;;           (.step v)))
                   serialization-not-supported)
 (type-transformer clojure.lang.LazySeq
                   false ;; 'LazySeq
                   false
                   serialization-not-supported)
 (type-transformer clojure.lang.PersistentList
                   'list
                   list
                   (fn [v]
                       (cons 'list (map type-transformer:to-code v))))
 (type-transformer clojure.lang.Symbol
                   'symbol
                   symbol
                   (fn [v]
                       (list 'symbol (str v))))
 (type-transformer clojure.lang.Keyword
                   'keyword
                   keyword
                   (fn [v]
                       (list 'keyword (keyword->string v))))
 (type-transformer clojure.lang.MapEntry
                   'map-entry
                   map-entry
                   (fn [v]
                       (list 'map-entry (key v) (val v))))
 (type-transformer database.store.Reference
                   'reference
                   reference
                   (fn [v]
                       (list 'reference (:hash v))))
 (type-transformer clojure.lang.PersistentVector
                   'vector
                   vector
                   (fn [v]
                       (cons 'vector (map type-transformer:to-code v))))
 (identityTransformer java.lang.Long)
 (identityTransformer java.lang.String)
 (identityTransformer java.lang.Boolean)
 (identityTransformer clojure.lang.Ratio)
 (identityTransformer java.lang.Double)
 (identityTransformer nil))



(defn type-transformer:to-code [obj]
  (let [t (type obj)]
    (if-let [tr (table-ref @type-transformers :type t)]
            ((:to-code tr) obj)
            (error "serialize: don't know about this type" t))))

(defn serialize [obj]
  (pr-str (type-transformer:to-code obj)))

(defn deserialize:eval [e]
  (cond (list? e)
        (apply (:constructor
                (table-ref @type-transformers :constructorname (first e)))
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


(defn store-put [obj]
  (let [s
        (serialize obj)
        hash
        (our-hash s)
        path
        (hash-path hash)]
    (spit-frugally path s)
    (Reference. hash)))

(defn store-get [ref]
  (-> (reference-path ref)
      (deserialize-file)))




