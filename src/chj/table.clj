(ns chj.table
    (:require [chj.debug :refer [p pseq]]
              [chj.util :refer [error hash-map-map]]))


(defn index
  ([entries key]
   (index entries key identity))
  ([entries key val]
   (apply hash-map
          (reduce (fn [res d]
                      (cons (key d)
                            (cons (val d)
                                  res)))
                  '()
                  entries))))


;; entries: a collection of 'rows'

;; indices: an atom with a map from field accessor keyword to a map
;; from field value to row value

(defrecord Table [entries indices])


(defn entries->table [entries]
  (Table. entries (atom {})))


(defn table-index-for [t key-name]
  (or (@(:indices t) key-name)
      ((swap! (:indices t)
              (fn [indices]
                  (conj indices
                        [key-name
                         (index (:entries t) key-name)])))
       key-name)))


(defn table-add [t val]
  (Table. (conj (:entries t) val)
          (atom
           (hash-map-map (fn [[key index]]
                             [key (conj index [(key val) val])])
                         @(:indices t)))))


(defn table-ref [t key-name key-val]
  ((table-index-for t key-name) key-val))


(defn table-indices [t]
  @(:indices t))

(defn table-indices-keyset [t]
  (set (keys @(:indices t))))

