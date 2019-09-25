(ns chj.io
    (:require [chj.util :refer [error]])
    (:import [java.io FileInputStream
                      FileOutputStream]))


;; (defn spit-bytes [path bytes]
;;   "Same as spit but write a ByteArray instead of a String"
;;   (with-open [out (FileOutputStream. path)]
;;              (.write out bytes)))

(defn spit-frugally [path bytes]
  "Same as spit but don't write to path if it already exists"
  ;; (and contains bytes, if content is different, an error is
  ;; thrown)?  Not done, that's for fsck.  XX is there an O_EXCL based
  ;; open in Java (couldn't find)?
  (try (with-open [in (FileInputStream. path)]
                  :already-saved)
       (catch java.io.FileNotFoundException e
              (spit path bytes))))


(defn mkdir [path]
  "Try to create path as a directory, return true if successful, false otherwise."
  ;; How to get the OS error, please? Also, name this try-mkdir or ?
  (.mkdir (java.io.File. path)))

