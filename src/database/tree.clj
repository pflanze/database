(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [database.store :as s]
              [chj.util :refer [flip vector-map]]))


(defn GET [x]
  (if (s/reference? x)
      (let [v (s/store-get x)]
        (assert (not (s/reference? v)))
        v)
      x))

(defn GET-deeply [x]
  (let [x* (GET x)]
    (if (vector? x*)
        (vector-map GET-deeply x*)
        x*)))


(def ^:dynamic *save?* false)

(defn PUT [x]
  (if *save?*
      (if (s/reference? x)
          x
          (s/store-put x))
      x))

(defn PUT-deeply [v]
  "This one is really specific to rb tree"
  (if (s/reference? v)
      v
      (match v

             nil
             v
             
             [color a x b]
             (PUT (if (and (s/reference? a)
                           (s/reference? b))
                      v
                      [color (PUT-deeply a)
                             x
                             (PUT-deeply b)]))

             :else
             (PUT v))))


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


(defn rb:balance-old [tree]
  (match tree
         ;;[:black nil [34 0] R]
         (:or [:black [:red [:red a x b] y c] z d]
              [:black [:red a x [:red b y c]] z d]
              [:black a x [:red [:red b y c] z d]]
              [:black a x [:red b y [:red c z d]]])
         [:red [:black a x b]
               y
               [:black c z d]]
         :else tree))

(defn rb:balance [tree]
  (let [cont
        (fn [a b c d x y z]
            [:red [:black a x b]
                  y
                  [:black c z d]])]
    (match (GET tree)
           [:black N1 z M1]
           (let [otherwise
                 (fn []
                     (match (GET M1)
                            [:red N2 z2 M2]
                            ;; [:black a  x [:red [:red b y c] z  d]]
                            ;; tree    N1 z M1    N2           z2 M2
                            (match (GET N2)
                                   [:red b y c]
                                   ;;    a  b c d  x y z
                                   (cont N1 b c M2 z y z2)
                                   :else
                                   (match (GET M2)
                                          [:red c z3 d]
                                          ;; [:black a  x [:red b  y  [:red c z  d]]]
                                          ;; tree    N1 z M1    N2 z2 M2    c z2 d
                                          (cont N1 N2 c d z z2 z3)
                                          :else
                                          tree))
                            :else
                            tree))]
             (match (GET N1)
                    [:red N2 y M2]
                    (match (GET N2)
                           ;; [:black [:red [:red a x b] y c] z d]
                           [:red a x b]
                           ;;    a b c  d
                           (cont a b M2 M1 x y z)

                           :else
                           (match (GET M2)
                                  [:red b y2 c]
                                  ;; [:black [:red a  x  [:red b y  c]] z d]
                                  ;; tree    N1    N2 y  M2    b y2 c   z M1
                                  ;;    a  b c d  x y  z
                                  (cont N2 b c M1 y y2 z)
                                  :else
                                  (otherwise)))
                    :else
                    (otherwise)))
           :else tree)))


(defn rb:add [tree k v]
  (let [ins
        (fn ins [tree]
            (let [tree (GET tree)]
              (match tree

                     nil
                     [:red nil (clojure.lang.MapEntry. k v) nil]

                     [color a kv b]
                     (cond
                      (< k (key kv)) (rb:balance [color (ins a) kv b])
                      (> k (key kv)) (rb:balance [color a kv (ins b)])
                      :else tree))))
        [_ a y b]
        (ins tree)]
    (PUT-deeply [:black a y b])))

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
                      (< k (key kv)) (recur a k)
                      (> k (key kv)) (recur b k)
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
                [_ a kv b] (let [k_ (key kv)]
                             (cond
                              (< k k_) (recur a k)
                              (> k k_) (recur b k)
                              :else (val kv))))))
  ([tree k]
   (rb:ref tree k nil)))


(defn rb:vals [tree]
  (match (GET tree)
         nil '()
         [_ a y b] (concat (rb:vals a)
                           (cons (val y) (rb:vals b)))))

(defn rb:keys [tree]
  (match (GET tree)
         nil '()
         [_ a y b] (concat (rb:keys a)
                           (cons (key y) (rb:keys b)))))

;; (defn dissoc [tree x])

;; (defn min-key [tree])
;; (defn max-key [tree])

;; (defn ceiling-key [tree x])
;; (defn floor-key [tree x])

;; (defn value-slice [tree min max])
