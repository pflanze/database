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
              [chj.threading :refer [defn* def*]]
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


;; Store

;; [String (Atom (Array Reference)) (Maybe (Atom Long)) (Maybe (Atom Long))]
(defrecord Store [path cache cache-hits cache-misses])

(def Store? (class-predicate-for Store))


;; Reference

;; [String Long (Atom no-val|value) (Atom Long)]
;; (long uses 64 bits)
(defrecord Reference [hash hashlong possibly-val deref-count])

;; deref-count: For "hotness" detection so that wenn evicting from
;; cache won't delete possibly-val.  This may be slow in cases with
;; contention. Will perhaps have to increment sparsely/rarely in
;; future optimization attempts, or perhaps use Java's
;; AtomicInteger. Or simply drop the plan and just go via cache all
;; the time (avoiding mutations to memory for reads completely!), but
;; then find out another way to avoid fighting evict situations (move
;; to second, smaller, cache instead of immediate dropping, then upon
;; reread move back to main cache and increment the count at that
;; time? (Use that count for the same purpose; less eviction?, and
;; keep val in possibly-val. ?) (Other idea: increment lifetime-count
;; randomly via modifying a random entry, or simply sequential, in the
;; cache. But that's not hotness of use.)

(def reference? (class-predicate-for Reference))

(defn reference= [a b]
  (or (identical? a b)
      (= (:hash a) (:hash b))))



(def no-val (gensym 'noval))

(defn reference*
  ([str long]
   (reference* str long no-val))
  ([str long val]
   (=> string? str)
   (=> int? long) ;; `int?` accepts longs and there's no `long?` predicate?
   (->Reference str long (atom val) (atom 0))))

(defn reference-possibly-val [ref]
  (inc! (:deref-count ref)) ;; even if there is no value, is fine
  @(:possibly-val ref))


(def string->hashlong)


;; When evicting an item from the cache, also delete its value
;; association, unless it's a "hot reference" (intensely used, might
;; be evicted because of a slot fight).

(def ^:dynamic *deref-count-cutoff* 500)

(defn cache-aset! [a i ref]
  (let [oldref (aget a i)]
    (if (and oldref
             (reference= oldref ref))
        nil ;; nothing to do; can happen when reinstating, or via other threads
        (do
            (aset a i ref)
            (if oldref
                (if (< @(:deref-count oldref) *deref-count-cutoff*)
                    (reset! (:possibly-val oldref) no-val)))))))

(defn* Store? cache-put-reference! [ref]
  "Cache a (freshly stored) reference"
  (let [
        a @(:cache this)
        siz (count a)
        hashlong (:hashlong ref)
        i (bit-and hashlong (dec siz))]
    (cache-aset! a i ref)
    ref))

(defn* Store? cache-maybe-get-reference [hashstr hashlong]
  "Try to retrieve an existing reference instance from the cache. Returns nill if not found."
  (let [
        a @(:cache this)
        siz (count a)
        i (bit-and hashlong (dec siz))]
    (if-let [r (aget a i)]
            (if (= (:hash r) hashstr)
                (do
                    (inc! (:cache-hits this))
                    r)))))

(defn* Store? reference [hashstr]
  "reference constructor for deserialisation (separate from reference*, do not allow multiple arguments to be safe for deserialisation)"
  (let [hashlong (string->hashlong hashstr)]
    (or (cache-maybe-get-reference hashstr hashlong)
        (reference* hashstr hashlong))))


;; Reference cache

;; xx
(def cache-start-size 256) ;; items, must be a power of 2

(defn make-cache
  ([]
   (make-cache cache-start-size))
  ([n]
   (make-array Reference n)))


(def _store-get-from-disk) ;; xx avoidable? re-order code

(defn* Store? cache-get [ref]
  "Looks up the given reference in the cache, if not found, puts ref into the cache and loads the vale from disk, if found reads the value from the cached ref, in either case stores the value into ref. Returns the value."
  (=> reference? ref)
  ;; ref always has its possibly-val unset when we get here.
  (let [
        a @(:cache this)
        siz (count a)
        i (bit-and (:hashlong ref) (dec siz))
        *ref (:possibly-val ref)]
    (letfn [(slowpath []
                      (let [v (_store-get-from-disk this ref)]
                        (reset! *ref v)
                        (cache-aset! a i ref)
                        (inc! (:cache-misses this))
                        v))]
           (if-let [r (aget a i)]
                   (if (reference= r ref)
                       (let [v @(:possibly-val r)]
                         (inc! (:cache-hits this))
                         (reset! *ref v)
                         ;;^ XX but now will have a copy of a reference
                         ;;  with also a hard pointer
                         ;;  XX actually set possibly-val in ref 
                         v)
                       (slowpath))
                   (slowpath)))))


;; xx
;; (defn cache-resize-up [v]
;;   (let [
;;         siz (count v)
;;         siz* (bit-shift-left siz 1)
;;         v* (make-array Reference siz*)]
;;     (loop [i 0]
;;           (if (< i siz)
;;               (do
;;                   ())))))



;; More Store

(defn open-store [path]
  (mkdir path)
  (->Store path
           (atom (make-cache))
           ;; xx make these optional, for speed-up:
           (atom 0)
           (atom 0)))

(defn store-statistics [the-store]
  [@(:cache-hits the-store) @(:cache-misses the-store)])

(defn store-statistics-reset! [the-store]
  (reset! (:cache-hits the-store) 0)
  (reset! (:cache-misses the-store) 0))


;; DatabaseCtx

;; Unlike Store which doesn't change for a particular backing store
;; (except for the cache, but that doesn't violate purity) and has no
;; (even functional) setters (and should be singletons per argument,
;; xx not enforced currently), DatabaseCtx carries additional information
;; that changes dynamically (i.e. it has setters).

(defrecord Database [the-store store?])

(def Database? (class-predicate-for Database))

(defn* Database? database-store?-set [b]
  (->Database (:the-store this)
              b))

(defn* Database? database-donotstore [] (database-store?-set false))
(defn* Database? database-dostore [] (database-store?-set true))

(defn* Database? path []
  (-> this :the-store :path))


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



(defn* Store? hash-path [hash]
  (str (:path this) "/" hash))

(defn* Store? reference-path [ref]
  (hash-path (:hash ref)))



(defrecord TypeTransformer [type constructorname constructor to-code])

(defn type-transformer [type constructorname constructor-1 to-code]
  (->TypeTransformer (=> type? type)
                     (=> (either symbol? string?) constructorname)
                     ;; ^ don't allow nil so that indexing
                     ;; constructorname works (without having to add
                     ;; functionality to ignore nil values)
                     (do
                         (=> (either function? nil?) constructor-1)
                         (fn [this args]
                             "Ignoring Database"
                             (apply constructor-1 args)))
                     (=> function? to-code)))

(defn type-transformer* [type constructorname constructor-2 to-code]
  "Same as type-transformer except that the data constructor gets access to the Database."
  ;; stupid mostly-COPY-PASTE
  (->TypeTransformer (=> type? type)
                     (=> (either symbol? string?) constructorname)
                     ;; ^ don't allow nil so that indexing
                     ;; constructorname works (without having to add
                     ;; functionality to ignore nil values)
                     (do
                         (=> (either function? nil?) constructor-2)
                         (fn [this args]
                             "Passing on Database"
                             (apply constructor-2 this args)))
                     (=> function? to-code)))

(def TypeTransformer? (class-predicate-for TypeTransformer))


(def type-transformers (atom (entries->table [])))

(defn add-transformer! [^TypeTransformer t]
  (=> TypeTransformer? t)
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
 (type-transformer* database.store.Reference
                    'reference
                    _reference
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

(defn* Store? deserialize:eval [e]
  (if (list? e)
      ((:constructor
        (table-ref @type-transformers :constructorname (first e)))
       this
       (map #(deserialize:eval %) (rest e)))
      e))

(defn* Store? deserialize-stream [s]
  (-> s
      java.io.InputStreamReader.
      java.io.PushbackReader.
      read ;; XX security
      deserialize:eval))

(defn* Store? deserialize-string [s]
  (-> s .getBytes ByteArrayInputStream. deserialize-stream))

(defn* Store? deserialize-file [path]
  (with-open [in (FileInputStream. path)]
             (deserialize-stream in)))


(defn* Store? store-put [obj]
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
        (hash-path hashstr)]
    (or (cache-maybe-get-reference hashstr hashlong)
        (do
            (spit-frugally path s)
            (cache-put-reference!
             (reference* hashstr hashlong obj))))))


(defn* Store? store-get-from-disk [ref]
  (-> (reference-path ref)
      (deserialize-file)))

(defn* Store? store-get [ref]
  (=> reference? ref)
  (let [possibly-val (reference-possibly-val ref)]
    (if (identical? possibly-val no-val)
        (cache-get ref)
        (do
            ;; Make sure the ref is in the cache again:
            (cache-put-reference! ref)
            possibly-val))))


;; XX implement an equality method in some way instead?
(defn store= [a b]
  (or (identical? a b)
      (if (reference? a)
          (if (reference? b)
              (reference= a b)
              false)
          (= a b))))

