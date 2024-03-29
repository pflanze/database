(ns chj.debug)

(def make-p-prefix "***")

(defn make-p [formatter-name formatter]
  (fn ([obj]
       (binding [*out* *err*]
                (print make-p-prefix)
                (print formatter-name)
                (print ": ")
                (prn (formatter obj)))
       obj)

      ([msg obj]
       (binding [*out* *err*]
                (print make-p-prefix)
                (print " ")
                (print msg)
                (print formatter-name)
                (print ": ")
                (prn (formatter obj)))
       obj)))

(def p (make-p "" identity))
(def pseq (make-p " (pseq)" seq))

