(ns lispboy.opcodes-test
  (:require [lispboy.opcodes :as sut]
            [clojure.test :refer :all]
            [clojure.spec.alpha :as s]))
(deftest byte-count-spec-test
  (testing "X isn't a valid byte count for instructions"
    (is (s/valid? ::sut/byte-count 1))
    (is (s/valid? ::sut/byte-count 2))
    (is (s/valid? ::sut/byte-count 3))
    (is (not (s/valid? ::sut/byte-count 4)))
    (is (not (s/valid? ::sut/byte-count 0)))
    (is (not (s/valid? ::sut/byte-count -51)))
    (is (not (s/valid? ::sut/byte-count 142)))
    (is (not (s/valid? ::sut/byte-count 2.5)))))
(deftest word16-opcode-spec-test
  (testing "X isn't a valid 16 bit opcode"
    (is  (s/valid? ::sut/word16-opcode  0xCBFD))
    (is  (not  (s/valid? ::sut/word16-opcode  0xCCFD)))))



(deftest opcode-spec-test
  (testing "X is invalid opcode"
    (is (s/valid? ::sut/opcode 0xCBFD))
    (is (s/valid? ::sut/opcode 0xCBFF))
    (is (s/valid? ::sut/opcode 0xCB00))
    (is (s/valid? ::sut/opcode 0x0000))
    (is (s/valid? ::sut/opcode 0x14))
    (is  (s/valid? ::sut/opcode 0xA7))
    (is (not  (s/valid? ::sut/opcode 0xCCFF)))
    (is (not  (s/valid? ::sut/opcode 0x0101))))
  )


(deftest opcode-data-spec-test
  (testing ""
      (is
       (s/valid? ::sut/opcode-data {:mnemonic "AND",
                                :bytes 1,
                                :cycles [4],
                                :operands [{:name "A", :immediate true}],
                                :immediate true,
                                :flags {:Z "Z", :N "0", :H "1", :C "0"}}))))
