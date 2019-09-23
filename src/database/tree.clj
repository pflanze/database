(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [database.store :as s]
              [chj.util :refer [flip]]))


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
  (match [tree]
         [(:or [:black [:red [:red a x b] y c] z d]
               [:black [:red a x [:red b y c]] z d]
               [:black a x [:red [:red b y c] z d]]
               [:black a x [:red b y [:red c z d]]])]
         [:red [:black a x b]
               y
               [:black c z d]]
         :else tree))


(defn rb:add [tree k v]
  (let [ins
        (fn ins [tree]
            (match tree
                   nil [:red nil (clojure.lang.MapEntry. k v) nil]
                   [color a kv b]
                   (cond
                    (< k (key kv)) (rb:balance [color (ins a) kv b])
                    (> k (key kv)) (rb:balance [color a kv (ins b)])
                    :else tree)))
        [_ a y b] (ins tree)]
    [:black a y b]))

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
  (match tree
         nil false
         [_ a kv b] (cond
                      (< k (key kv)) (recur a k)
                      (> k (key kv)) (recur b k)
                      :else true)))


(defn rb:ref
  "Check if the key is present and return value or a default"
  ([tree k not-found]
   (loop [tree tree
          k k]
     (match tree
            nil not-found
            [_ a kv b] (cond
                         (< k (key kv)) (recur a k)
                         (> k (key kv)) (recur b k)
                         :else (val kv)))))
  ([tree k]
   (rb:ref tree k nil)))


(defn rb:vals [tree]
  (match tree
         nil '()
         [_ a y b] (concat (rb:vals a)
                           (cons (val y) (rb:vals b)))))

(defn rb:keys [tree]
  (match tree
         nil '()
         [_ a y b] (concat (rb:keys a)
                           (cons (key y) (rb:keys b)))))

;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
