(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [database.store :as s]
              [chj.util :refer [flip]]))


(defn GET [x]
  (if (s/reference? x)
      (s/get x)
      x))


(def ^:dynamic *save?* false)

(defn PUT [x]
  (if *save?*
      (if (s/reference? x)
          x
          (s/put x))
      x))


(defn KEY [x]
  (key (GET x)))

(defn VAL [x]
  (val (GET x)))


;; License

;; Copyright © 2016 Henry Garner, 2019 Christian Jaeger & others (see
;; Git commit history)

;; Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.


(defn rb:depth [tree]
  "The max depth of the tree, for debugging purposes"
  (match (GET tree)
         nil
         0
         [color a kv b]
         (inc (max (rb:depth a)
                   (rb:depth b)))))


(defn rb:balance [tree]
  (match (GET tree)
         (:or [:black [:red [:red a x b] y c] z d]
              [:black [:red a x [:red b y c]] z d]
              [:black a x [:red [:red b y c] z d]]
              [:black a x [:red b y [:red c z d]]])
         (PUT [:red (PUT [:black a x b])
                    y
                    (PUT [:black c z d])])
         :else tree))


(defn rb:add [tree k v]
  (let [ins
        (fn ins [tree]
            (match (GET tree)
                   nil [:red nil (clojure.lang.MapEntry. k v) nil]
                   [color a kv b]
                   (cond
                    (< k (KEY kv)) (rb:balance [color (PUT (ins a)) kv b])
                    (> k (KEY kv)) (rb:balance [color a kv (PUT (ins b))])
                    :else tree)))
        [_ a y b]
        (ins tree)]
    (PUT [:black a y b])))

(defn rb:conj [tree [k v]]
  (rb:add tree k v))



;; XX name, or how ?
(defn rb:into [tree pairs]
  (reduce rb:conj tree pairs))

(defn seq->rb [s]
  (rb:into nil s))


(defn rb:contains?
  "Check if the key is present"
  [tree k]
  (match (GET tree)
         nil false
         [_ a kv b] (cond
                      (< k (KEY kv)) (recur a k)
                      (> k (KEY kv)) (recur b k)
                      :else true)))


(defn rb:ref
  "Check if the key is present and return value or a default"
  ([tree k not-found]
   (loop [tree
          tree
          k
          k]
         (match (GET tree)
                nil not-found
                [_ a kv b] (let [k_ (KEY kv)]
                             (cond
                              (< k k_) (recur a k)
                              (> k k_) (recur b k)
                              :else (VAL kv))))))
  ([tree k]
   (rb:ref tree k nil)))


(defn rb:vals [tree]
  (match (GET tree)
         nil '()
         [_ a y b] (concat (rb:vals a)
                           (cons (VAL y) (rb:vals b)))))

(defn rb:keys [tree]
  (match (GET tree)
         nil '()
         [_ a y b] (concat (rb:keys a)
                           (cons (KEY y) (rb:keys b)))))

;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
