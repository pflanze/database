(ns database.store
    ;;(:require [clojure.core :exclude [get]]) nope,
    (:refer-clojure :exclude [get])
    (:import [java.io ByteArrayInputStream
                      ByteArrayOutputStream
                      ObjectOutputStream
                      ObjectInputStream
                      FileOutputStream
                      FileInputStream]
             [java.security MessageDigest]
             [java.math BigInteger]))


;; lib

(defn spit-bytes [path bytes]
  "Same as spit but write a ByteArray instead of a String"
  (with-open [out (FileOutputStream. path)]
             (.write out bytes)))

;; /lib


(defrecord Store [path])

(def the-store (Store. "db/"))



(def hash-algorithm
     (MessageDigest/getInstance "SHA-256"))

(defn our-hash [bytes]
  (.digest hash-algorithm bytes))

(defn hash->hex [bytes]
  (format "%032x" (BigInteger. 1 bytes)))


(defrecord Reference [hash])


(defn hash-path [hash]
  (str (:path the-store) "/" (hash->hex hash)))

(defn reference-path [ref]
  (hash-path (:hash ref)))



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


(defn put [obj]
  (let [bytes
        (serialize obj)
        hash
        (our-hash bytes)
        path
        (hash-path hash)]
    (spit-bytes path bytes)
    (Reference. hash)))

(defn get [ref]
  (-> (reference-path ref)
      (deserialize-file)))




