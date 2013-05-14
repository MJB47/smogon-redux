
(ns smogon.core
  "Load all the services!"
  (:require (smogon dex web repl gaybot)))

(defn start-all
  [& {:keys [start-dex start-web start-repl setup-user-ns start-gaybot]
      :or {start-dex true, start-web true, start-repl true, setup-user-ns true, start-gaybot true}}]
  (when start-dex 
    (smogon.dex/start-dex))
  (when start-web 
    (smogon.web/start-web))
  (when start-repl 
    (smogon.repl/start-repl))
  (when setup-user-ns
    (smogon.repl/setup-user-ns))
  (when start-gaybot
    (smogon.gaybot/start-gaybot)))
