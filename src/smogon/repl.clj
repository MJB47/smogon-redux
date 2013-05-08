
(ns smogon.repl
  (:require [taoensso.timbre :as log]
            [clojure.tools.nrepl.server :as nrepl.server]))

(defn setup-user-ns
  "Set up fun and useful bindings in the user namespace, so the poor person
  developing this program doesn't get RSI from repeatedly importing stuff."
  []
  (binding [*ns* (find-ns 'user)]
    (use 'clojure.repl)
    (require '[clojure.core.logic :as l])
    (use 'smogon.dex)))

(defn start-repl
  "Start an embedded nREPL server. This is intended for production use; generally
  during development you will simply use Leiningen's jack-in REPL.

  Generally you should prefer smogon.core/start-all with :start-repl true."
  [& {:keys [port], :or {port 1234}}]
  (nrepl.server/start-server :port port)
  (log/info "Starting Smogon nREPL on port" port))

