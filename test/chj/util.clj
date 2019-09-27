(ns test.chj.util
    (:require [chj.util :refer [xconj]]
              [chj.test :refer [is*]]
              [clojure.test :refer :all]))


(deftest t-xconj
  (is*

   (= (xconj {} [1 2])
      {1 2})

   (thrown-with-msg? Exception #"key already in map: 1"
                     (-> {}
                         (xconj [1 2])
                         (xconj [1 3])))

   (= (-> #{}
          (xconj [1 2])
          (xconj [1 3]))
      #{[1 2] [1 3]})

   (thrown-with-msg? Exception #"value already in collection: \[1 2\]"
                     (-> #{}
                         (xconj [1 2])
                         (xconj [1 3])
                         (xconj [1 2])))))

