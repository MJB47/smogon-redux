
(defproject smogon "0.1.0-SNAPSHOT"
  :description "Pokemon, occasionally on the internet."
  :url "http://www.smogon.com/"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/core.logic "0.8.3"]
                 [org.clojure/tools.nrepl "0.2.2"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/tools.macro "0.1.2"]
                 [enlive "1.1.1"]
                 [compojure "1.1.5"]
                 [http-kit "2.0.1"]
                 [com.taoensso/timbre "1.5.2"] ;; Logger
                 [ring "1.1.8"]
                 [potemkin "0.2.2"]
                 [criterium "0.4.1"]
                 ;; for nrepl.el
                 [clojure-complete "0.2.3"]]

  :repl-options {:history-file ~(str (user/leiningen-home) "/smogon/repl-history")
                 :init (do (use 'smogon.core)
                           (start-all :start-repl false))}
  
  :compile-path ~(str (user/leiningen-home)  "/smogon/target/classes")
  :native-path ~(str (user/leiningen-home)  "/smogon/target/native")
  :target-path ~(str (user/leiningen-home) "/smogon/target"))
