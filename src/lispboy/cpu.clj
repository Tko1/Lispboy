(ns lispboy.cpu
  (:require [lispboy.simple-lens :as l]
            [clojure.java [io :as io]]
            ;; This gives us raw-opcode-data
            [lispboy.util :refer [bitmask] :as util]
            [lispboy.cartridge :as c]
            [lispboy.opcodes :refer [raw-opcode-data] :as op]
            [clojure.spec.alpha :as s]))
;; TODO figure the appropriate place and way to do this 
(s/check-asserts true)
;;;;;;;;;;;;;;;;;;;;;;;;;;  Specs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::cpu (s/keys :req-un [::a ::f ::c ::b ::e ::d ::l ::h ::sp ::pc]))
(s/def ::gameboy-filename
  (s/and string?
         (s/or :ends-with-gb
               #(clojure.string/ends-with? % "gb")
               :ends-with-gbc
               #(clojure.string/ends-with? % "gbc"))))
(s/def ::gameboy-instruction-arg
  (s/or :no-arg nil?
        :word8-arg ::util/word8
        :word16-arg ::util/word16))

(def cpu
  (s/assert ::cpu
            {:a 0x0 :f 0x0
             :c 0x0 :b 0x0
             :e 0x0 :d 0x0
             :l 0x0 :h 0x0
             :sp 0x0 :pc 0x0}))

(defn slurp-bytes
  [x]
  (with-open [out (java.io.ByteArrayOutputStream. )]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn to-unsigned [x]
  (bit-and x 0xff))


(defn file-to-cartridge [file]
  {:pre  [(s/assert ::gameboy-filename file)]
   :post [(s/assert ::c/cartridge %)]}
  (->> (slurp-bytes file) (mapv to-unsigned)))

(def tetris-file-name "Tetris.gb")
(def tetris-cartridge (file-to-cartridge tetris-file-name))

(defn get-byte-at [cartridge ind]
  {:pre [(s/assert ::c/cartridge cartridge)
         (s/assert integer? ind)]
   :post [(s/assert ::util/word8 %)]}
  (nth cartridge ind))
(def cartridge-title-start-index 0x134)
(def cartridge-title-end-index 0x143)
(defn get-cartridge-title [cartridge]
  {:pre [(s/assert ::c/cartridge cartridge)]
   :post [(s/assert string? %)]}
  (->> (range cartridge-title-start-index
              cartridge-title-end-index)
       (map #(char (get-byte-at cartridge %)))
       (reduce str)))
(def cartridge-instruction-start-index 0x100)
(defn jp [cpu address]
  {:pre [(s/assert ::cpu cpu)
         (s/assert int? address)]
   :post [(s/assert ::cpu %)]}
  (assoc cpu :pc address))
(defn jp-a16 [cpu word16]
  {:pre [(s/assert ::cpu cpu)
         (s/assert ::util/word16 word16)]
   :post [(s/assert ::cpu %)]}
  (jp cpu word16))
;; Reads opcode and args
;; Returns [opcode,  word8 | word16 argument]
;; TODO separate this spec? It seems very specific to this area 
(s/def ::instruction-bytes
  (s/or
   :no-args (s/tuple ::op/opcode)
   :arg (s/tuple ::op/opcode ::gameboy-instruction-arg)))
(defn read-instruction-bytes [cartridge n]
  {:pre [(s/assert ::c/cartridge cartridge)
         (s/assert int? n)]
   :post [(s/assert ::instruction-bytes %)]}
  (let [opcode (s/assert ::util/word8 (-> cartridge (get-byte-at n)))
        opcode-data (s/assert
                     map?
                     (-> raw-opcode-data :unprefixed (get opcode)))
        bytes       (s/assert ::op/byte-count
                              (-> opcode-data :bytes))
        arg        (s/assert
                    ::gameboy-instruction-arg
                    (case bytes
                      1
                      nil
                      2
                      (get-byte-at cartridge (+ n 1))
                      3 
                      (util/word8-pair-to-word16
                       [ (get-byte-at cartridge (+ n 1))
                        (get-byte-at cartridge (+ n 2))])))]

    (if arg [opcode arg] [opcode])))

(defn run-instruction-bytes [cpu [raw-opcode arg :as instr-bytes]]
  {:pre [(s/assert ::cpu cpu)
         (s/assert ::instruction-bytes instr-bytes)]
   :post [(s/assert ::cpu %)] }
  (let [opcode (-> raw-opcode-data :unprefixed (get raw-opcode))
        mnemonic (-> opcode :mnemonic)
        operands-printer (fn [ops]
                           (->> ops
                                (map :name)
                                (reduce #(str % "," %2) "")))
        command           (case mnemonic
                            "JP"
                            jp
                            nil)
        changed-cpu            (cond
                                 (and command arg)
                                 (command cpu arg)

                                 command
                                 (command cpu)

                                 :else
                                 cpu)

        changed-cpu-debug-print (fn [command arg]
                                  (if command
                                    (print "Defined! " )
                                    (print "Error: Undefined "))
                                  (println
                                   (format
                                    "Instruction %x; %s %s (Arg: %s)(Z: %s N: %s H: %s C: %s)"
                                    raw-opcode
                                    mnemonic
                                    (-> opcode :operands operands-printer)
                                    arg
                                    (-> opcode :flags :Z)
                                    (-> opcode :flags :N)
                                    (-> opcode :flags :H)
                                    (-> opcode :flags :C)
                                    ))
                                  (println "Bytes: " (-> opcode :bytes) ", Cycles: " (-> opcode :cycles))
                                  (println "Cpu before: " cpu)
                                  (println "Cpu after: " changed-cpu))]

    (changed-cpu-debug-print command arg)
    (println "Update code, press y to continue, anything else to quit")
    (if (=  "y" (read-line))
      (-> cpu (assoc :continue true))
      (-> cpu (assoc :continue false )))))

(defn run-instruction-at-pc [cpu cartridge]
  {:pre [(s/assert ::cpu cpu)
         (s/assert ::c/cartridge cartridge)]
   :post [(s/assert ::cpu %)]}
  (let [instr-bytes (read-instruction-bytes cartridge (:pc cpu))]
    (run-instruction-bytes cpu instr-bytes)))

(run-instruction-bytes cpu (read-instruction-bytes tetris-cartridge 0x100))
(defn run-cartridge-recur [cpu cartridge]
  {:pre [(s/assert ::cpu cpu)
         (s/assert ::c/cartridge cartridge)]
   :post [(s/assert ::cpu %)]}
  (loop [cpu cpu
         cartridge cartridge]
    (if (:continue cpu)
      (recur (-> cpu
                 (run-instruction-at-pc cartridge)
                 (update :pc + 1))
             cartridge)
      (-> cpu
          (run-instruction-at-pc cartridge)
          (update :pc + 1)))))
(defn run-cartridge [cpu cartridge]
  {:pre [(s/assert ::c/cartridge cartridge)]}
  (-> cpu
      (assoc :pc cartridge-instruction-start-index)
      (assoc :continue true)
      (run-cartridge-recur cartridge)))
(run-cartridge cpu tetris-cartridge)
