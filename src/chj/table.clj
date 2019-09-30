(ns chj.table
    (:require [chj.debug :refer [p pseq]]
              [chj.util :refer [error hash-map-map xconj]]))


(defn unique-index-add [idx key row]
  (let [val (key row)]
    (if (contains? idx val)
        (error "unique index already contains an entry with this field, value"
               key val)
        (conj idx [val row]))))

(defn entries->unique-index [entries key]
  (reduce #(unique-index-add %1 key %2)
          {}
          entries))


;; entries: a collection of 'rows'

;; indices: an atom with a map from field accessor keyword to a map
;; from field value to row value

(defrecord Table [entries indices])


(defn entries->table [entries]
  (Table. entries (atom {})))


(defn table-unique-index-for [t key-name]
  (or (@(:indices t) key-name)
      ((swap! (:indices t)
              (fn [indices]
                  (conj indices
                        [key-name
                         (entries->unique-index (:entries t) key-name)])))
       key-name)))


(defn table-add [t row]
  (Table. (xconj (:entries t) row)
          (atom
           (hash-map-map
            (fn [[key idx]]
                [key (unique-index-add idx key row)])
            @(:indices t)))))


(defn table-ref [t key-name key-val]
  ((table-unique-index-for t key-name) key-val))


(defn table-indices [t]
  @(:indices t))

(defn table-indices-keyset [t]
  (set (keys @(:indices t))))

