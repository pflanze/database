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

(defn store [obj]
  (let [s (pr-str obj)
          hash (our-hash s)
          path (str (:path the-store) "/" hash)]
    (spit path s)
    (make-Reference hash)))


(defn dereference [ref]
  (let [path (str (:path the-store) "/" (:hash ref))]
    ;; XXX security
    (-> (slurp path)
        (read-string))))
