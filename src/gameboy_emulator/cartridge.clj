(ns emulator.cartridge)

(require '[hask-tools.functor :refer :all])
(require '[hask-tools.debug :refer :all])
(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.monad.parser :refer :all])

(require '[hask-tools.monad.maybe :refer :all])
(require '[hask-tools.monad.transformer.maybet :refer :all])

(require '[clojure.spec.alpha :as s])
(require '[hask-tools.util :refer :all])
(require '[hask-tools.bytes :refer :all])
(require '[hask-tools.cpu :refer :all])
(require '[hask-tools.lens :refer :all])
(require '[emulator.instruction :refer :all])
(def opcode16-bit-header 0xCB)
;;;;;;;;;;;;;;;;;;;;;;;; Specs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defenum ::bit-size :bits8 :bits16 :no-bits)

(s/def ::cartridge (s/coll-of :hask-tools.bytes/word8))
;;;;;;;;;;;;;;;;;;;;;;;; Readers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Primitive
(def read-bytep
  (add-parser-type ::cartridge :hask-tools.bytes/word8
                   (specp :hask-tools.bytes/word8)))

(defn read-bitsp [bitsize]
  { :pre [ (s/valid? ::bit-size bitsize)]}
  (cond
    (= bitsize :bits8)
    read-bytep

    (= bitsize :no-bits)
    noparse
    
    (= bitsize :bits16)
    (domonad 
     (f <- read-bytep)
     (s <- read-bytep)
     (return-parser (word8-pair-to-word16 [f s])))))
(def read-opcodep
  (add-parser-type ::cartridge :hask-tools.cpu/opcode
  (domonad
   (f <- read-bytep)
   (if (= f opcode16-bit-header)
     (domonad
      (s <- read-bytep)
      (return-parser (word8-pair-to-word16 [s f])))
     (return-parser f)))))
((:parse read-opcodep) [0xcb 0x0f 0xff 0x10 0x01 0x11])
0xcb0f
(word8-pair-to-word16 [0x0f 0x01])
0x010f
((:parse (read-bitsp :bits16)) [0xf0 0x0f 0xff 0x10 0x01 0x11])

(def read-argument8p
  (add-parser-type ::cartridge :hask-tools.cpu/arg
                   read-bytep))

(def read-argument16p
    (add-parser-type ::cartridge :hask-tools.cpu/arg
                     (read-bitsp :bits16)))

(defn read-raw-instructionp [ instr-dataset ]
  (domonad
   (opcode <- read-opcodep)
   (dlet [ instr-dataset- (get-instruction-dataset instr-dataset)
          instr-data    (l-get
                         (linstruction-data-lookup opcode)
                         instr-dataset-)
          error          (if (not instr-data)
                           (throw (Exception. (str "Error: no instruction with opcode " opcode))))
          arg-data       (l-get larg-data instr-data)

          arg-bits (map #(case % 0 :no-bits 1 :bits8 2 :bits16) arg-data)
          ;; MaybeT Parser Arg
          arg-parsers
          (map read-bitsp arg-bits)
          
          arg-parser
          (vec-sequencep
           arg-parsers)]
        
         (domonad
          (args <- arg-parser)           
          (return-parser [opcode args])))))
(defn instructionp [ instr-set]
  (fmap (read-instructionp instr-set) #(->Instruction (first %) (second %))))
(def cpu-instructionp [ cpu ]
  (instructionp (get-instruction-dataset cpu))) 
(def gameboy-instructionp
  (instructionp gameboy-instruction-set))
((:parse gameboy-instructionp)
 [ 0x12 0x12 0x32 ])
(defn try-read-instruction [ instr-dataset ]
  (:run-maybet
   (domonad
    (opcode <- (lift :MaybeT read-opcodep))
    (dlet [ instr-dataset- (get-instruction-dataset instr-dataset)
          instr-data    (l-get
                         (linstruction-data-lookup opcode)
                         instr-dataset-)
          error          (if (not instr-data) (println "Error: no instruction with opcode " opcode))
          arg-data       (l-get larg-data instr-data)

          arg-bits (map #(case % 1 :bits8 2 :bits16) arg-data)
          ;; MaybeT Parser Arg
          arg-parsers
          (map #(lift :MaybeT (read-bitsp %)) arg-bits)
          
          arg-parser
          (vec-sequencep
           arg-parsers)]
      (domonad
       (args <- arg-parser)
       (lift :MaybeT (return-parser [opcode args])))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Fns 
(defn slurp-bytes
  [x]
  (with-open [out (java.io.ByteArrayOutputStream. )]
    (clojure.java.io/copy (clojure.java.io/input-stream x) out)
    (.toByteArray out)))
(defn to-unsigned [x]
  (bit-and x 0xff))
(defn file-to-cartridge [file]
  (->> (slurp-bytes file) (mapv to-unsigned)))
(def +tetris-file-name+ "Tetris.gb")
(def tetris-cartridge (file-to-cartridge +tetris-file-name+))

(def +pokemon-yellow-file-name+ "PokemonYellow.gbc")
(def pokemon-yellow-cartridge (file-to-cartridge +pokemon-yellow-file-name+))


(sdefn read-game-title [(::cartridge cartridge)]
      (reduce str (map #(char (nth cartridge %))(range 0x134 0x143))))
(read-game-title tetris-cartridge)

(read-game-title (file-to-cartridge "PokemonYellow.gbc"))

(sdefn get-byte-at [(::cartridge cartridge) (integer? ind)]
       (nth cartridge ind))
(defenum ::cartridge-type :gameboy :gameboy-color)
(sdefn get-cartridge-type [(::cartridge cartridge)]
       (-> ::cartridge-type)
       (case (get-byte-at pokemon-yellow-cartridge 0x143)
         0x80
         :gameboy-color

         :gameboy))
(defenum ::gb-sgb-indicator :gameboy :super-gameboy)
(sdefn get-gb-sgb-indicator [(::cartridge cartridge)]
       (-> ::gb-sgb-indicator)
       (case (get-byte-at pokemon-yellow-cartridge 0x143)
         0x00
         :gameboy
         0x03
         :super-gameboy))
(s/def ::detailed-cartridge-int
  #(member? % [0 1 2 3 5 6 8 9 0xB 0xC 0xD 0x12 0x13 0x19 0x1A 0x1B 0x1C 0x1D 0x1E 0x1F 0xFD 0xFE 0xF 0x10 0x11 0xFF]))
(defn detailed-cartridge-int-to-meta [x]
  (get
   {0 #{:ROM}
    1 #{:ROM :MBC1}
    2 #{:ROM :MBC1 :RAM}
    3 #{:ROM :MBC1 :RAM :BATT}
    5 #{:ROM :MBC2}
    6 #{:ROM :MBC2 :BATT}
    8 #{:ROM :RAM}}
   x))
(sdefn get-cartridge-detailed-int [(::cartridge cartridge) ]
       (get-byte-at cartridge 0x147))
(get-cartridge-detailed-int tetris-cartridge)


(comment   ( (:parse (:run-maybet (->MaybeT (return-parser (Just 1)))))  [0x0f 0xff 0xcb])
    (->MaybeT (return-parser (Just (+ opcode opcode2))
  ((:parse (:run-maybet (lift :MaybeT read-opcodep))) [0x0f]))))
;;((:parse (try-read-instruction 12)) [ 0x0a 0xf0 0xff])


