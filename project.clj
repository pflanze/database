(defproject database "0.0.1"
  :description "A simple object database"
  :url "http://github.com/pflanze/database"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[mvxcvi/multihash "2.0.3"]
                 ;; [org.clojure/clojure "1.11.0"]
                 ;; [org.clojure/clojure "1.10.0"]
                 ;; [org.clojure/clojure-contrib "1.1.0"]
                 ]
  ;; :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]]
  :main database.main
  :repl-options {:init-ns database.main}
  ;;:repl-options {}
  
  )

