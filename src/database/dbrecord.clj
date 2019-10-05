(ns database.dbrecord
    (:require [chj.debug :refer [p pseq]]
              [database.store :as s]
              [clojure.string :as str]
              [chj.util :refer [with-gensym]]))


;; Easily define record data types to be stored in the database


(defn defdbrecord-expand [definitions  name fieldnames]
  (if (= fieldnames (name @definitions))
      ;; The same dbrecord was already defined with the same fieldnames
      ;; last time, don't do anything (to avoid (= (name) (name))
      ;; breaking)
      nil

      (let [classname
            (let [namestr
                  (str name)
                  capnamestr
                  (str/capitalize namestr)]
              (symbol (if (= namestr capnamestr)
                          (str "_" name)
                          capnamestr)))
            classnamedot
            (symbol (str classname "."))]
        (swap! definitions #(conj % [name fieldnames]))
        (with-gensym
         v
         `(do
              ;; record
              (defrecord ~classname ~fieldnames)

              ;; constructor *function*
              (defn ~name ~fieldnames
                (~classnamedot ~@fieldnames))

            ;; predicate
            (defn ~(symbol (str name "?")) [v#]
              (instance? ~classname v#))

            ;; make serializable
            (s/add-transformer!
             (s/type-transformer
              ~classname
              '~name
              ~name
              (fn [~v]
                  (list '~name
                        ~@(map (fn [fieldname]
                                   `(s/type-transformer:to-code
                                     (~(keyword fieldname) ~v)))
                               fieldnames))))))))))


(def definitions (atom {}))

(defmacro defdbrecord [name fieldnames]
  (defdbrecord-expand definitions  name fieldnames))

