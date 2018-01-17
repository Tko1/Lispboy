(ns hask-tools.core
  (:gen-class))
(require '[hask-tools.debug :refer :all])
(require '[hask-tools.util :refer :all])
(require '[hask-tools.lens :refer :all])

(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.monad.maybe :refer :all])

(require '[clojure.spec.alpha :as s])
(require '[emulator.instruction :refer :all])
(require '[hask-tools.bytes :refer :all])
  
;;TODO oh duh, use protocols for lens..

(defn -main [& args]
  (println "HI! ")
  (println "Test"))


