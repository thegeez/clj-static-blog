(ns clj-static-blog.dev-server
  (:use [ring.adapter.jetty :only [run-jetty]]
        [ring.middleware.file :only [wrap-file]]
        [ring.middleware.stacktrace :only [wrap-stacktrace]])
  (:require [clj-static-blog.core :as core]))

(defn wrap-generate [handler]
  (fn [req]
    (core/generate)
    (handler req)))

(defn run-server []
  (let [target (:target (read-string (slurp "settings.clj")))]
    (run-jetty
     (-> (fn [req])
         (wrap-file target)
         wrap-stacktrace
         wrap-generate)
     {:port 4000 :join? false})))
