(ns database.store
    (:require [clojure.core.match :refer [match]])
    (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))


(import 'java.security.MessageDigest
        'java.math.BigInteger
        'java.io.ObjectOutputStream
        'java.io.ObjectInputStream
        'java.io.FileOutputStream
        'java.io.FileInputStream
        'java.nio.file.Files
        'java.nio.file.Paths)

;; lib

(def make-p-prefix "***")

(defn make-p [formatter-name formatter]
  (fn ([obj]
       (print make-p-prefix)
       (print formatter-name)
       (print ": ")
       (prn (formatter obj))
       obj)

      ([msg obj]
       (print make-p-prefix)
       (print " ")
       (print msg)
       (print formatter-name)
       (print ": ")
       (prn (formatter obj))
       obj)))

(def p (make-p "" identity))
(def pseq (make-p " (pseq)" seq))


(defn spit-bytes [path bytes]
  "Same as spit but write a ByteArray instead of a String"
  (with-open [out (FileOutputStream. path)]
             (.write out bytes)))

;; /lib



(defn make-Store [path]
  {:type 'Store :path path})

(defn make-Reference [hash]
  {:type 'Reference :hash hash})


(def the-store (make-Store "db/"))

(defn our-hash [bytes]
  (let [algorithm
        (MessageDigest/getInstance "SHA-256")
        raw
        (.digest algorithm bytes)]
    (format "%032x" (BigInteger. 1 raw))))

(defn serialize [obj]
  (let [out
        (ByteArrayOutputStream. 4096)
        writer
        (ObjectOutputStream. out)]
    (.writeObject writer obj)
    (.close writer)
    (.close out)
    (.toByteArray out)))

(defn deserialize-stream [in]
  (let [reader
        (ObjectInputStream. in)]
    (.readObject reader)))

(defn deserialize [bytes]
  (deserialize-stream (ByteArrayInputStream. bytes)))

(defn deserialize-file [path]
  (with-open [in (FileInputStream. path)]
             (deserialize-stream in)))


(defn store [obj]
  (let [bytes
        (serialize obj)
        hash
        (our-hash bytes)
        path
        (str (:path the-store) "/" hash)]
    (spit-bytes path bytes)
    (make-Reference hash)))

(defn dereference [ref]
  (-> (str (:path the-store) "/" (:hash ref))
      (deserialize-file)))




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
