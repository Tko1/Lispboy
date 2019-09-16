(ns lispboy.cpu
  (:require [lispboy.simple-lens :as l]
            [clojure.java [io :as io]]
            ;; This gives us raw-opcode-data
            [lispboy.util :refer [bitmask] :as util]
            [lispboy.cartridge :as c]
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
;; Creates a predicate to check if a value is a wordn, where n
;; can be, say, word8 for a 8 bit value, word16 for 16 bit..



(defn slurp-bytes
  [x]
  (with-open [out (java.io.ByteArrayOutputStream. )]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))

(defn to-unsigned [x]
  (bit-and x 0xff))

(s/def ::gameboy-filename
  (s/and string?
         (s/or :ends-with-gb
               #(clojure.string/ends-with? % "gb")
               :ends-with-gbc
               #(clojure.string/ends-with? % "gbc"))))
(defn file-to-cartridge [file]
  {:pre  [(s/valid? ::gameboy-filename file)]
   :post [(s/valid? ::c/cartridge %)]}
  (->> (slurp-bytes file) (mapv to-unsigned)))

(def tetris-file-name "Tetris.gb")
(def tetris-cartridge (file-to-cartridge tetris-file-name))

(defn get-byte-at [cartridge ind]
  {:pre [(s/valid? ::c/cartridge cartridge)
         (s/valid? integer? ind)]
   :post [(s/valid? ::util/word8 %)]}
  (nth cartridge ind))
(def cartridge-title-start-index 0x134)
(def cartridge-title-end-index 0x143)
(defn get-cartridge-title [cartridge]
  {:pre [(s/valid? ::c/cartridge cartridge)]
   :post [(s/valid? string? %)]}
  (->> (range cartridge-title-start-index
              cartridge-title-end-index)
       (map #(char (get-byte-at cartridge %)))
       (reduce str)))
(def cartridge-instruction-start-index 0x100)
(defn jp [cpu address]
  {:pre [(s/valid? ::cpu cpu)
         (s/valid? int? address)]
   :post [(s/valid? ::cpu %)]}
  (assoc cpu :pc address))
(defn jp-a16 [cpu word16]
  {:pre [(s/valid? ::cpu cpu)
         (s/valid? ::util/word16 word16)]
   :post [(s/valid? ::cpu %)]}
  (jp cpu word16))
;; Reads opcode and args 
(defn read-instruction-bytes [cartridge n]
  (let [opcode (s/assert ::util/word8 (-> cartridge (get-byte-at n)))
        opcode-data (s/assert
                     map?
                     (-> raw-opcode-data :unprefixed (get opcode)))
        bytes       (s/assert ::util/word8 (-> opcode-data :bytes))
        args        (s/assert (s/coll-of ::util/word8)
                              (as-> bytes it
                                (- it 1)
                                (range it)
                                (map #(+ n %) it)
                                (mapv #(get-byte-at cartridge %) it)))]
    
    (into [] (concat [opcode] args))))
(defn run-instruction-bytes [cpu [raw-opcode & args]]
  (let [opcode (-> raw-opcode-data :unprefixed (get raw-opcode))
        operands-printer (fn [ops]
                           (->> ops
                                (map :name)
                                (reduce #(str % "," %2) "")))]
    (println
     (format
      "Error: Undefined instruction %x; %s %s (Args: %s)(Z: %s N: %s H: %s C: %s)"
      raw-opcode
      (-> opcode :mnemonic)
      (-> opcode :operands operands-printer)
      args
      (-> opcode :flags :Z)
      (-> opcode :flags :N)
      (-> opcode :flags :H)
      (-> opcode :flags :C)
      ))
    (println "Bytes: " (-> opcode :bytes) ", Cycles: " (-> opcode :cycles))
    (println "Cpu: " cpu)
    (println "Update code, press y to continue, anything else to quit")
    (if (=  "y" (read-line))
      (-> cpu (assoc :continue true))
      (-> cpu (assoc :continue false )))))

(defn run-instruction-at-pc [cpu cartridge]
  {:pre [(s/valid? ::cpu cpu)
         (s/valid? ::c/cartridge cartridge)]
   :post [(s/valid? ::cpu %)]}
  (let [instr-bytes (read-instruction-bytes cartridge (:pc cpu))]
    (run-instruction-bytes cpu instr-bytes)))

(run-instruction-bytes cpu (read-instruction-bytes tetris-cartridge 0x100))
(defn run-cartridge-recur [cpu cartridge]
  {:pre [(s/valid? ::cpu cpu)
         (s/valid? ::c/cartridge cartridge)]
   :post [(s/valid? ::cpu %)]}
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
  {:pre [(s/valid? ::c/cartridge cartridge)]}
  (-> cpu
      (assoc :pc cartridge-instruction-start-index)
      (assoc :continue true)
      (run-cartridge-recur cartridge)))
(run-cartridge cpu tetris-cartridge)
