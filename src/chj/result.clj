(ns chj.result
    (:require [chj.util :refer [error xconj xpartition with-gensym]]
              [clojure.core.match :refer [match]]
              [chj.debug :refer [p]]))


(defprotocol Result
  (value [x])
  (error-value [x]))

(deftype Ok [value]
  Result

  (value [s] value))

(deftype Err [error-value]
  Result

  (error-value [s] error-value))


(defn Ok? [v] (instance? Ok v))
(defn Err? [v] (instance? Err v))


;; don't use? Use ->Ok and ->Err instead?
(defn ok [v] (Ok. v))
(defn err [v] (Err. v))
;; /


(defn match-Result-error [v]
  ;; XX now follow Result interface, first.
  (error "match-Result: not a Result" v))

(defn match-Result-no-match [v]
  (error "match-Result: no match for" v))

(defmacro match-Result
  ([expr & cases]
   (let [m (reduce (fn [m [mat expr]]
                       (xconj m (match (apply vector mat)
                                       ['Ok v]
                                       [:ok [v expr]]
                                       ['Err v]
                                       [:err [v expr]])))
                   {}
                   (xpartition 2 cases))]
     (with-gensym
      V
      `(let [~V ~expr]
         (if (Ok? ~V)
             ~(if-let [[v expr] (:ok m)]
                      `(let [~v (value ~V)]
                         ~expr)
                      `(match-Result-no-match ~V))
             ~(if-let [[v expr] (:err m)]
                      `(if (Err? ~V)
                           (let [~v (error-value ~V)]
                             ~expr)
                           (match-Result-error ~V))
                      `(match-Result-no-match ~V))))))))


(defmacro if-Ok
  ([expr then else]
   `(match-Result ~expr
                  (~'Ok ~'it) ~then
                  (~'Err ~'it) ~else))
  ([expr then]
   `(if-Ok ~expr ~then nil)))

