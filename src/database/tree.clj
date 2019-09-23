(ns database.tree
    (:require [clojure.core.match :refer [match]]
              [database.store :as s]
              [chj.util :refer [flip]]))


(defn GET [x]
  (if (s/reference? x)
      (let [v (s/get x)]
        (assert (not (s/reference? v)))
        v)
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


;; (defn rb:balance [tree]
;;   (match (GET tree)
;;          (:or [:black [:red [:red a x b] y c] z d]
;;               [:black [:red a x [:red b y c]] z d]
;;               [:black a x [:red [:red b y c] z d]]
;;               [:black a x [:red b y [:red c z d]]])
;;          (PUT [:red (PUT [:black a x b])
;;                     y
;;                     (PUT [:black c z d])])
;;          :else tree))

(defn rb:balance [tree]
  (let [cont
        (fn [a b c d x y z]
            (PUT [:red (PUT [:black a x b])
                       y
                       (PUT [:black c z d])]))]
    (match (GET tree)
           [:black N1 z M1]
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
                                tree))
                  :else
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
                         tree))
           :else tree)))


(defn rb:add [tree k v]
  (let [ins
        (fn ins [tree]
            (match tree

                   nil
                   [:red nil (clojure.lang.MapEntry. k v) nil]

                   [color a kv b]
                   (cond
                    ;; XX optimize: recursively store only after known to need it
                    (< k (KEY kv)) (rb:balance [color (PUT (ins (GET a))) kv b])
                    (> k (KEY kv)) (rb:balance [color a kv (PUT (ins (GET b)))])
                    :else tree)))
        [_ a y b]
        ;; XX ditto, unavoidable to GET here since rb:balance does PUT
        (GET
         (ins (GET tree)))]
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
