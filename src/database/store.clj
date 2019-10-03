(ns database.store
    (:require [chj.debug :refer [p pseq]]
              [chj.io :refer [spit-frugally mkdir]]
              [chj.util :refer [class-predicate-for
                                for-each
                                error
                                ->* ->>*
                                => either
                                inc!
                                keyword->string
                                type?
                                map-entry]]
              [chj.table :refer [entries->table table-add table-ref]]
              [clojure.test :refer [function?]])
    (:import [java.io ByteArrayInputStream
                      ByteArrayOutputStream
                      ObjectOutputStream
                      ObjectInputStream
                      FileOutputStream
                      FileInputStream]
             [java.security MessageDigest]
             [java.math BigInteger]
             [java.util Base64]))


;; Mechanism:

;; A reference is a representation of a stored value (via hash).
;; It can also contain a pointer to the actual value (in memory).

;; XX then it is cleared occasionally XX (avoid using WeakRef ?)

;; The cache is just randomized



;; Reference

;; [String Long (Atom no-val|value)]
;; (long uses 64 bits)
(defrecord Reference [hash hashlong possibly-val])

(def no-val (gensym 'noval))

(defn reference*
  ([str long]
   (reference* str long no-val))
  ([str long val]
   (assert (string? str))
   (assert (int? long)) ;; `int?` accepts longs and there's no `long?` predicate?
   (->Reference str long (atom val))))

(def string->hashlong)
(defn reference [str]
  "reference constructor for deserialisation (separate from reference*, do not allow multiple arguments to be safe for deserialisation)"
  (reference* str (string->hashlong str)))

(def reference? (class-predicate-for Reference))

(defn reference= [a b]
  (= (:hash a) (:hash b)))


;; Reference cache

;; xx
(def referenceCache-start-size 16) ;; items, must be a power of 2

(defn make-referenceCache
  ([]
   (make-referenceCache referenceCache-start-size))
  ([n]
   (make-array Reference n)))


(def store-get-from-disk)

(defn referenceCache-get [the-store ref]
  (assert (reference? ref))
  ;; ref always has its possibly-val unset when we get here.
  (let [
        a @(:cache the-store)
        siz (count a)
        i (bit-and (:hashlong ref) (dec siz))]
    (letfn [(slowpath []
                      (let [v (store-get-from-disk the-store ref)]
                        (reset! (:possibly-val ref) v)
                        ;; XX now, we did set the possibly-val and
                        ;; have a stron ref now, which leaks forever,
                        ;; this still needs dealing with.
                        (inc! (:referenceCache-misses the-store))
                        (aset a i ref)
                        v))]
           (if-let [r (aget a i)]
                   (if (reference= r ref)
                       (do
                           (inc! (:referenceCache-hits the-store))
                           @(:possibly-val r)
                         ;;^ XX but now will have a copy of a reference
                         ;;  with also a hard pointer
                         ;;  XX actually set possibly-val in ref 
                         )
                       (slowpath))
                   (slowpath)))))

;; xx
;; (defn referenceCache-resize-up [v]
;;   (let [
;;         siz (count v)
;;         siz* (bit-shift-left siz 1)
;;         v* (make-array Reference siz*)]
;;     (loop [i 0]
;;           (if (< i siz)
;;               (do
;;                   ())))))



;; Store

;; [String (Atom (Array Reference)) (Maybe (Atom Long)) (Maybe (Atom Long))]
(defrecord Store [path cache referenceCache-hits referenceCache-misses])

(def Store? (class-predicate-for Store))

(defn open-store [path]
  (mkdir path)
  (->Store path
           (atom (make-referenceCache))
           ;; xx make these optional, for speed-up:
           (atom 0)
           (atom 0)))

(defn store-statistics [the-store]
  [@(:referenceCache-hits the-store) @(:referenceCache-misses the-store)])

(defn store-statistics-reset! [the-store]
  (reset! (:referenceCache-hits the-store) 0)
  (reset! (:referenceCache-misses the-store) 0))


;; DatabaseCtx

;; Unlike Store which doesn't change for a particular backing store
;; (except for the cache, but that doesn't violate purity) and has no
;; (even functional) setters (and should be singletons per argument,
;; xx not enforced currently), DatabaseCtx carries additional information
;; that changes dynamically (i.e. it has setters).

(defrecord DatabaseCtx [the-store store?])

(defn DatabaseCtx-store?-set [c b]
  (->DatabaseCtx (:the-store c)
                 b))

(defn DatabaseCtx-donotstore [c] (DatabaseCtx-store?-set c false))
(defn DatabaseCtx-dostore [c] (DatabaseCtx-store?-set c true))



;; Object names (hashing)

(defn chop [s]
  (.substring s 0 (dec (.length s))))

(defn rawhash->string [bytes]
  (-> (.encodeToString (Base64/getEncoder) bytes)
      (.replace \/ \_)
      (chop)))


(defn byte-signed-to-unsigned [b]
  ;; (if (< b 0) (+ b 256) b)
  (bit-and b 255))

(defn rawhash->hashlong [bytes]
  (letfn [(get [i]
               (bit-shift-left (byte-signed-to-unsigned (aget bytes i))
                               (bit-shift-left i 3)))]
         (+ (get 0) (get 1) (get 2) (get 3) (get 4) (get 5) (get 6))))


(defn string->rawhash [s]
  (.decode (Base64/getDecoder) (-> s
                                   (.replace \_ \/)
                                   (str \=))))

(def string->hashlong (->* string->rawhash rawhash->hashlong))


(def hash-algorithm
     (MessageDigest/getInstance "SHA-256"))

(defn our-hash [str]
  (.digest hash-algorithm (.getBytes str)))



(defn hash-path [the-store hash]
  (assert (Store? the-store))
  (str (:path the-store) "/" hash))

(defn reference-path [the-store ref]
  (hash-path the-store (:hash ref)))



(defrecord TypeTransformer [type constructorname constructor to-code])

(defn type-transformer [type constructorname constructor to-code]
  (->TypeTransformer (=> type? type)
                     (=> (either symbol? string?) constructorname)
                     ;; ^ don't allow nil so that indexing
                     ;; constructorname works (without having to add
                     ;; functionality to ignore nil values)
                     (=> (either function? nil?) constructor)
                     (=> function? to-code)))

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
                    (str typ)
                    nil
                    identity))

(declare type-transformer:to-code)

(defn serialization-not-supported [v]
  (error "serialization of this type is not supported" (type v)))

(add-transformers!
 (type-transformer clojure.lang.LongRange
                   "range"
                   nil
                   ;; (fn [v]
                   ;;     (list 'range
                   ;;           (.first v)
                   ;; Oops, no accessors for those:
                   ;;           (.end v)
                   ;;           (.step v)))
                   serialization-not-supported)
 (type-transformer clojure.lang.LazySeq
                   "LazySeq"
                   nil
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


(defn store-put [the-store obj]
  (assert (Store? the-store))
  (assert (not (Store? obj)))
  (let [s
        (serialize obj)
        rawhash
        (our-hash s)
        hashstr
        (rawhash->string rawhash)
        hashlong
        (rawhash->hashlong rawhash)
        path
        (hash-path the-store hashstr)]
    (spit-frugally path s)
    (reference* hashstr hashlong obj)))


(defn store-get-from-disk [the-store ref]
  (-> (reference-path the-store ref)
      (deserialize-file)))

(defn store-get [the-store ref]
  (assert (Store? the-store))
  (assert (reference? ref))
  (let [a (:possibly-val ref)  possibly-val @a]
    (if (identical? possibly-val no-val)
        (reset! a (referenceCache-get the-store ref))
        possibly-val)))


;; XX implement an equality method in some way instead?
(defn store= [a b]
  (or (identical? a b)
      (if (reference? a)
          (if (reference? b)
              (reference= a b)
              false)
          (= a b))))

