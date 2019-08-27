


(defn make-Store [path]
  {:type 'Store :path path})

(defn make-Reference [hash]
  {:type 'Reference :hash hash})


(def the-store (make-Store "db/"))


(defn store [obj]
  (let [s (pr-str obj)
          hash (str (hash s))
          path (str (:path the-store) "/" hash)]
    (spit path s)
    (make-Reference hash)))


(defn dereference [ref]
  (let [path (str (:path the-store) "/" (:hash ref))]
    ;; XXX security
    (-> (slurp path)
        (read-string))))
