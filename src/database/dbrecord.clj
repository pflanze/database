(ns database.dbrecord
    (:require [chj.debug :refer [p pseq]]
              [database.store :as s]
              [clojure.string :as str]
              [chj.util :refer [error with-gensym]]))


;; Easily define record data types to be stored in the database


(defn defdbrecord-expand [definitions  name fieldnames]
  "Like defrecord but also:

- If name starts with a lower-case character, still define the class
  with a capitalized Name, but also define name as an alias to ->Name
  (giving Name to defdbrecord means you're going to write your own
  constructor wrapper, or use ->Name directly)

- Registers with the database.store so that records of this type can
  be stored.

- If defdbrecord was called already with the same name and fieldnames
  last time, it does nothing; this is so that `=` returns true on
  objects created before and after reloading files with defdbrecord
  forms.

"
  (if (= fieldnames (name @definitions))
      ;; The same dbrecord was already defined with the same fieldnames
      ;; last time, don't do anything
      nil

      (let [[classname
             was-ucfirst?]
            (let [namestr
                  (str name)
                  capnamestr
                  (str/capitalize namestr)]
              [(symbol capnamestr)
               (= namestr capnamestr)])

            ->classname
            (symbol (str "->" classname))

            constructorname
            (if was-ucfirst?
                ->classname
                name)]
        
        (swap! definitions #(conj % [name fieldnames]))

        (with-gensym
         v
         `(do
              ;; record
              (defrecord ~classname ~fieldnames)

              ;; alias constructor function
              ~@(if was-ucfirst?
                    nil
                    `((def ~name ~->classname)))

              ;; predicate
              (defn ~(symbol (str name "?")) [v#]
                (instance? ~classname v#))

              ;; make serializable
              (s/add-transformer!
               (s/type-transformer
                ~classname
                '~constructorname
                ~constructorname
                (fn [~v]
                    (list '~constructorname
                          ~@(map (fn [fieldname]
                                     `(s/type-transformer:to-code
                                       (~(keyword fieldname) ~v)))
                                 fieldnames))))))))))


(def definitions (atom {}))

(defmacro defdbrecord [name fieldnames]
  (defdbrecord-expand definitions  name fieldnames))

