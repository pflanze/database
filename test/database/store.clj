(ns test.database.store
    (:require [clojure.test :refer :all]
              [database.store :as s]
              [chj.test :refer [is*]]))


(deftest hashing

  (def h (s/our-hash ""))
   ;; $ echo -n|sha256sum 
   ;; e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855

  (is*
   (= (s/rawhash->hashlong h)
      ;; #x1cfc9842c4b0e3      
      8159030233313507)

   (= (s/rawhash->string h)
      "47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU")

   ;;(not (= (string->rawhash "47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU") h))
   ;; oh well
   (= (-> (s/string->rawhash "47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU")
          s/rawhash->string)
      "47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU")
   
   (= (s/string->hashlong "47DEQpj8HBSa+_TImW+5JCeuQeRkm5NMpJWZG3hSuFU")
      8159030233313507)

   (= (s/rawhash->string (s/our-hash "hello"))
      "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ")

   (= (s/string->hashlong "LPJNul+wow4m6DsqxbninhsWHlwfp0JecwQzYpOLmCQ")
      46074346397889068)))



(deftest unbroken-symbol-serialisation

  (def this (s/open-store "db"))

  (is*

   (= (->> (symbol "a b") s/store-put s/store-get type)
      clojure.lang.Symbol)

   (= (->> (symbol "a b") s/store-put s/store-get)
      (symbol "a b"))))

