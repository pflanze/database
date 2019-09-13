

(defn spit-bytes [path bytes]
  "Same as spit but write a ByteArray instead of a String"
  (with-open [out (FileOutputStream. path)]
             (.write out bytes)))
