(ns database.store
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



(defn our-hash [bytes]
  (let [algorithm
        (MessageDigest/getInstance "SHA-256")
        raw
        (.digest algorithm bytes)]
    raw))

(defn hash->hex [bytes]
  (format "%032x" (BigInteger. 1 bytes)))


(defrecord Reference [hash])

(defn reference-path [ref]
  (str (:path the-store) "/" (hash->hex (:hash ref))))


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
        (str (:path the-store) "/" (hash->hex hash))]
    (spit-bytes path bytes)
    (Reference. hash)))

(defn dereference [ref]
  (-> (reference-path ref)
      (deserialize-file)))




