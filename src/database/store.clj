(ns database.store
    (:require [clojure.core.match :refer [match]])
    (:require [cognitect.transit :as transit])
    (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))


(import 'java.security.MessageDigest
        'java.math.BigInteger)



(defn make-Store [path]
  {:type 'Store :path path})

(defn make-Reference [hash]
  {:type 'Reference :hash hash})


(def the-store (make-Store "db/"))

(defn our-hash [^String s]
  (let [algorithm (MessageDigest/getInstance "SHA-256")
                  raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn serialize [obj]
  (def out (ByteArrayOutputStream. 4096))
  (def writer (transit/writer out :json))
  (transit/write writer obj)
  (.toString out))

(defn deserialize [str]
  ;; InputStream stream =
  ;;  new ByteArrayInputStream(exampleString.getBytes(StandardCharsets.UTF_8));
  (def in (ByteArrayInputStream. (.getBytes str)))
  (def reader (transit/reader in :json))
  (transit/read reader))



(defn store [obj]
  (let [s (serialize obj)
          hash (our-hash s)
          path (str (:path the-store) "/" hash)]
    (spit path s)
    (make-Reference hash)))


(defn dereference [ref]
  (let [path (str (:path the-store) "/" (:hash ref))]
    (-> (slurp path)
        (deserialize))))




;; License

;; Copyright © 2016 Henry Garner

;; Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.



(defn rb:balance [tree]
  (match [tree]
         [(:or [:black [:red [:red a x b] y c] z d]
               [:black [:red a x [:red b y c]] z d]
               [:black a x [:red [:red b y c] z d]]
               [:black a x [:red b y [:red c z d]]])]
         [:red [:black a x b]
               y
               [:black c z d]]
         :else tree))

(defn rb:assoc [tree k v]
  (let [ins
        (fn ins [tree]
            (match tree
                   nil [:red nil (clojure.lang.MapEntry. k v) nil]
                   [color a kv b]
                   (cond
                    (< k (key kv)) (rb:balance [color (ins a) kv b])
                    (> k (key kv)) (rb:balance [color a kv (ins b)])
                    :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

(defn rb:contains? [tree k]
  (match tree
         nil false
         [_ a kv b] (cond
                     (< k (key kv)) (recur a k)
                     (> k (key kv)) (recur b k)
                     :else true)))

(defn rb:vals [tree]
  (match tree
         nil '()
         [_ a y b] (concat (rb:vals a)
                           (cons (val y) (rb:vals b)))))

(defn rb:keys [tree]
  (match tree
         nil '()
         [_ a y b] (concat (rb:keys a)
                           (cons (key y) (rb:keys b)))))

;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
