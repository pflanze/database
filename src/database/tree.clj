(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [database.store :as s]))


(defrecord Pair [car cdr])

(defn pair [a r]
     (Pair. a r))

(s/add-transformer!
 (s/type-transformer database.tree.Pair 'pair pair
                     (fn [v]
                         (list 'pair
                               (s/type-transformer:to-code (:car v))
                               (s/type-transformer:to-code (:cdr v))))))

;; (is (-> (Pair. 10 (Pair. 20 30)) s/put s/get)
;;     (Pair. 10 (Pair. 20 30)))


;; License

;; Copyright © 2016 Henry Garner

;; Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.



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

(defn rb:assoc [tree k v]
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

(defn rb:contains? [tree k]
  (match tree
         nil false
         [_ a kv b] (cond
                     (< k (key kv)) (recur a k)
                     (> k (key kv)) (recur b k)
                     :else true)))

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
