
(ns smogon.web
  (:require [taoensso.timbre :as log]
            [org.httpkit.server :as httpkit.server]
            (compojure [core :refer :all] route handler)
            ring.middleware.stacktrace
            [smogon.dexweb :as dexweb]))

(defroutes app-routes
  (GET "/" [] "Hello World")
  #'dexweb/dex-routes
  (compojure.route/files "" {:root "public"})
  (compojure.route/not-found "Not Found"))

(def app (-> #'app-routes
             compojure.handler/site
             ring.middleware.stacktrace/wrap-stacktrace-log))

(defn start-web
  "Don't call this function directly; prefer smogon.core/start-all with
  :start-web true"
  [& {:keys [port], :or {port 9001}}]
  (httpkit.server/run-server #'app {:port port})
  (log/info "Starting Smogon web server on port" port))
