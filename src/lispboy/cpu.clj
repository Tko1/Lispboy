(ns lispboy.cpu
  (:require [lispboy.simple-lens :as l]
            [clojure.java [io :as io]]
            ;; This gives us raw-opcode-data
            [lispboy.opcodes :refer :all]
            [clojure.spec.alpha :as s]))

(s/def ::cpu (s/keys :req-un [::a ::f ::c ::b ::e ::d ::l ::h ::sp ::pc]))
(def cpu
  (s/assert ::cpu
            {:a 0x0 :f 0x0
             :c 0x0 :b 0x0
             :e 0x0 :d 0x0
             :l 0x0 :h 0x0
             :sp 0x0 :pc 0x0}))
