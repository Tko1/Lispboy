(ns lispboy.cpu-test
  (:require [lispboy.cpu :as sut]
            [lispboy.opcodes :as op]
            [clojure.test :refer :all]))

(deftest test-ld-dispatch--ld-b-d
  (testing "Cpu's :b should equal its initial :d, 300"
    (let [start-cpu (-> sut/cpu (assoc :b 45) (assoc :d 300))
          ld-opcode-data   (get-in op/raw-opcode-data [:unprefixed 0x42])
          ld-fn    (sut/ld-dispatch ld-opcode-data)
          new-cpu (ld-fn start-cpu)]
      (is (= (:b new-cpu) (:d new-cpu)) ":d and :b should match in new cpu")
      (is (= (:d new-cpu) 300) ":d should be 300")
      (is (= (:b new-cpu) 300) ":b should be 300"))))
