(ns server
  (:require [org.httpkit.server :refer [run-server with-channel on-receive on-close send!]]))

(defonce channels (atom #{}))

(defn ws-handler [request]
  (with-channel request channel
    (swap! channels conj channel)
    (on-close channel (fn [status]
                        (swap! channels disj channel)
                        (println "Channel closed:" status)))
    (on-receive channel (fn [data]
                          (println "Received:" data)
                          (doseq [c @channels]
                            (when-not (= c channel) ;; Don't echo to sender
                              (send! c data)))))))

(defn -main [& args]
  (let [port 8080]
    (run-server ws-handler {:port port})
    (println "WebSocket relay server started on port" port)))

(apply -main *command-line-args*)
