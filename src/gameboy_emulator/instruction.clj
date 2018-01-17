(ns emulator.instruction)
(require '[hask-tools.util :refer :all])
(require '[hask-tools.lens :refer :all])
(require '[clojure.spec.alpha :as s])
(require '[emulator.common-lens :refer :all])


(defn    arg-val-data [n] (s/and #(<= % n) #(>= % 0)))
(s/def ::arg-data
  (s/and (s/coll-of #(and (integer? %) (>= % 0)))
         #(vector? %)))
(assert (s/valid? ::arg-data [1 2 3 4 5 8]))
(assert (not (s/valid? ::arg-data [1 2 -1])))

(spec-defrecord ArgData [ (::arg-data arg-data) ])
(make-lenses 'ArgData)
(defn arg-data [n] (s/and (s/coll-of (arg-val-data n))
                          #(vector? %)))
(def new-ArgData-n
  (curry-n
   2
   (fn [n argd]
     { :post [ (s/valid? (arg-data n) (:arg-data %))]}
     (new-ArgData argd))))
(def new-gameboy-ArgData (new-ArgData-n 2))
(s/def ::gameboy-arg-data (arg-data 2))
(assert (s/valid? ::gameboy-arg-data [1 0 0 1 2 2 2 1 2]))
(assert (not (s/valid? ::gameboy-arg-data [0 0 -1])))
(assert (not (s/valid? ::gameboy-arg-data [0 0 1 3])))
(assert (not (s/valid? ::gameboy-arg-data '(0 0 1))))


(defrecord Instruction [ op-code args])
(make-lenses 'Instruction)
(defn new-Instruction--raw [[op-code args]]
  (->Instruction op-code args))

(spec-defrecord InstructionData [(string?   name)
                                 (integer?   op-code)
                                 (::arg-data arg-data)
                                 (integer?   cycles)
                                 (fn?        action)])
;; Emphasizes that InstructionData is the type
(spec-defn new-gameboy-InstructionData
           [(string?   name)
            (integer?   op-code)
            (::gameboy-arg-data arg-data)
            (integer?   cycles)
            (fn?        action)]
           (new-InstructionData name op-code arg-data cycles action))
(assert (new-gameboy-InstructionData "LD A,(BC)" 0x0A [0] 8 (fn [_] 1)))
(make-lenses 'InstructionData)

(s/def ::instruction-dataset (s/map-of integer? #(= type InstructionData)))

(try-gen-field-protocol 'instruction-dataset)

(def-prlens linstruction-dataset
  get-instruction-dataset set-instruction-dataset)

(defn linstruction-data-lookup [ opcode ]
  { :pre [ (integer? opcode)]}
  (l-comp linstruction-dataset
           (rlens #(get % opcode)
                  #(assoc % opcode %2))))
(extend-type clojure.lang.PersistentArrayMap
  HasInstruction-dataset
  (get-instruction-dataset [x] x)
  (set-instruction-dataset [whole part] part))


(def gameboy-instruction-set
  {
   0x00 (new-gameboy-InstructionData "NOP"    0x00 [0] 0 (fn [instr cpu] cpu))
   0x12 (new-gameboy-InstructionData "LD (DE),A"   0x12  [2] 8 (fn [instr cpu] ))
   })
(comment (def gameboy-instruction-set
  {
   0x00 (new-gameboy-InstructionData "NOP"    0x00 [0] 0 (fn [instr cpu] cpu))
   })
  (comment 
  { 0x01 (new-gameboy-InstructionData "LD BC,d16" 0x01 [2] 12 (ld-n-nn :cb))
   0x02 (new-gameboy-InstructionData "LD (BC),A" 0x02 [0] 8 (lda-r1a-r2 :cb :a))
   0x03 (new-gameboy-InstructionData "INC BC"    0x03 [0] 8 (inc-nn :cb))
   0x04 (new-gameboy-InstructionData "INC B"    0x04 [0] 4 (inc-n :b))
   0x05 (new-gameboy-InstructionData "DEC B"    0x05 [0] 4 (dec-n :b)) ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; LD NN,N
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   0x06 (new-gameboy-InstructionData "LD B,n" 0x06 [1] 8 (ld-nn-n :b))
   ;; RLCA
   0x07 (new-gameboy-InstructionData "RLCA"   0x07 [0] 4 (rlca))
   ;; LD (nn),sp
   0x08 (new-gameboy-InstructionData "LD (nn),sp" 0x08 [2] 20 (lda16-nna-r2 :sp))
   ;;ADD r1-r2
   0x09 (new-gameboy-InstructionData "ADD HL,BC" 0x09  [0] 8  (add16-r1-r2 :lh :cb))
   0x0A (new-gameboy-InstructionData "LD A,(BC)" 0x0A  [0] 8  (lda8-r1-r2a :a :cb))
   ;;DEC
   0x0B (new-gameboy-InstructionData "DEC BC" 0x0B [0] 8 (dec-nn :cb))
   0x0C (new-gameboy-InstructionData "INC C" 0x0C  [0] 8  (inc-n :c))
   0x0D (new-gameboy-InstructionData "DEC C" 0x0D  [0] 8  (dec-n :c))
   ;;14
   0x0E (new-gameboy-InstructionData "LD C,n" 0x0E [1] 8 (ld-nn-n :c))
   0x0F (new-gameboy-InstructionData "RRCA"   0x0F  [0] 8  (rrca))
   ;;STOP
   0x10 (new-gameboy-InstructionData "STOP"   0x10  [1] 4  (stop))

   0x11 (new-gameboy-InstructionData "LD DE,d16"   0x11  [2] 12 (ld-n-nn :ed))
   0x12 (new-gameboy-InstructionData "LD (DE),A"   0x12  [2] 8 (lda8-r1a-r2 :ed))
   ;;INC
   0x13 (new-gameboy-InstructionData "INC DE" 0x13  [0] 8 (inc-nn :ed))
   0x14 (new-gameboy-InstructionData "INC D" 0x14  [0] 4 (inc-n :d))
  ;; 0x15
   ;;22
   0x16 (new-gameboy-InstructionData "LD D,n" 0x16 [1] 8 (ld-nn-n :d))
   ;;30
   0x1E (new-gameboy-InstructionData "LD E,n" 0x1E [1] 8 (ld-nn-n :e))
   0x24 (new-gameboy-InstructionData "INC H" [0]  0x24  [0] 4 (inc-n :h))
   ;;38
   0x26 (new-gameboy-InstructionData "LD H,n" 0x26 [1] 8 (ld-nn-n :h))
   0x2E (new-gameboy-InstructionData "LD L,n" 0x2E [1] 8 (ld-nn-n :l))
   ;;INC
   0x34 (new-gameboy-InstructionData "INC (HL)" 0x34  [0] 8 (inc-nn :ed))
   ;;DEC
   0x35 (new-gameboy-InstructionData "DEC (HL)" 0x35  [0] 12 (deca-nn :lh))

   0x36 (new-gameboy-InstructionData "LD (HL),d8" 0x36  [1] 8 (lda8-r1a-n :lh))

   
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; LD R1,R2
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
   0x40 (new-gameboy-InstructionData "LD B,B" 0x40 [0] 4 (ld-r1-r2 :b :b))
   0x41 (new-gameboy-InstructionData "LD B,C" 0x41 [0] 4 (ld-r1-r2 :b :c))
   0x42 (new-gameboy-InstructionData "LD B,D" 0x42 [0] 4 (ld-r1-r2 :b :d))
   0x43 (new-gameboy-InstructionData "LD B,E" 0x43 [0] 4 (ld-r1-r2 :b :e))
   0x44 (new-gameboy-InstructionData "LD B,H" 0x44 [0] 4 (ld-r1-r2 :b :h))
   0x45 (new-gameboy-InstructionData "LD B,L" 0x45 [0] 4 (ld-r1-r2 :b :l))
   ;;TODO ?
   0x46 (new-gameboy-InstructionData "LD B,(HL)" 0x46 [0] 8 (lda8-r1-r2a :b :lh))

   0x47 (new-gameboy-InstructionData "LD B,A" 0x46 [0] 4 (ld-r1-r2 :b :a))
   0x48 (new-gameboy-InstructionData "LD C,B" 0x48 [0] 4 (ld-r1-r2 :c :b))
   0x49 (new-gameboy-InstructionData "LD C,C" 0x49 [0] 4 (ld-r1-r2 :c :c))
   0x4A (new-gameboy-InstructionData "LD C,D" 0x4A [0] 4 (ld-r1-r2 :c :d))
   0x4B (new-gameboy-InstructionData "LD C,E" 0x4B [0] 4 (ld-r1-r2 :c :e))
   0x4C (new-gameboy-InstructionData "LD C,H" 0x4C [0] 4 (ld-r1-r2 :c :h))
   0x4D (new-gameboy-InstructionData "LD C,L" 0x4D [0] 4 (ld-r1-r2 :c :l))
   ;;TODO?
   0x4E (new-gameboy-InstructionData "LD C,(HL)" 0x4E [0] 8 (lda8-r1-r2a :c :lh))

   0x50 (new-gameboy-InstructionData "LD D,B" 0x50 [0] 4 (ld-r1-r2 :d :b))
   0x51 (new-gameboy-InstructionData "LD D,C" 0x51 [0] 4 (ld-r1-r2 :d :c))
   0x52 (new-gameboy-InstructionData "LD D,D" 0x52 [0] 4 (ld-r1-r2 :d :d))
   0x53 (new-gameboy-InstructionData "LD D,E" 0x53 [0] 4 (ld-r1-r2 :d :e))
   0x54 (new-gameboy-InstructionData "LD D,H" 0x54 [0] 4 (ld-r1-r2 :d :h))
   0x55 (new-gameboy-InstructionData "LD D,L" 0x55 [0] 4 (ld-r1-r2 :d :l))
   ;;TODO
   0x56 (new-gameboy-InstructionData "LD D,(HL)" 0x56 [0] 8 (lda8-r1-r2a :d :lh))

   0x57 (new-gameboy-InstructionData "LD D,A" 0x57 [0] 4 (ld-r1-r2 :d :a))
   0x58 (new-gameboy-InstructionData "LD E,B" 0x58 [0] 4 (ld-r1-r2 :e :b))
   0x59 (new-gameboy-InstructionData "LD E,C" 0x59 [0] 4 (ld-r1-r2 :e :c))
   0x5A (new-gameboy-InstructionData "LD E,D" 0x5A [0] 4 (ld-r1-r2 :e :d))
   0x5B (new-gameboy-InstructionData "LD E,E" 0x5B [0] 4 (ld-r1-r2 :e :e))
   0x5C (new-gameboy-InstructionData "LD E,H" 0x5C [0] 4 (ld-r1-r2 :e :h))
   0x5D (new-gameboy-InstructionData "LD E,L" 0x5D [0] 4 (ld-r1-r2 :e :l))
   ;;TODO
   0x5E (new-gameboy-InstructionData "LD E,(HL)" 0x5E [0] 8 (lda8-r1-r2a :e :lh))

   0x5F (new-gameboy-InstructionData "LD E,A" 0x5F [0] 4 (ld-r1-r2 :e :a))
   
   0x60 (new-gameboy-InstructionData "LD H,B" 0x60 [0] 4 (ld-r1-r2 :h :b))
   0x61 (new-gameboy-InstructionData "LD H,C" 0x61 [0] 4 (ld-r1-r2 :h :c))
   0x62 (new-gameboy-InstructionData "LD H,D" 0x62 [0] 4 (ld-r1-r2 :h :d))
   0x63 (new-gameboy-InstructionData "LD H,E" 0x63 [0] 4 (ld-r1-r2 :h :e))
   0x64 (new-gameboy-InstructionData "LD H,H" 0x64 [0] 4 (ld-r1-r2 :h :h))
   0x65 (new-gameboy-InstructionData "LD H,L" 0x65 [0] 4 (ld-r1-r2 :h :l))
   
   0x66 (new-gameboy-InstructionData "LD H,(HL)" 0x66 [0] 8 (lda8-r1-r2a :h :lh))

   0x67 (new-gameboy-InstructionData "LD H,A" 0x66 [0] 4 (ld-r1-r2 :h :a))
   0x68 (new-gameboy-InstructionData "LD L,b" 0x68 [0] 4 (ld-r1-r2 :l :b))
   0x69 (new-gameboy-InstructionData "LD L,c" 0x69 [0] 4 (ld-r1-r2 :l :c))
   0x6A (new-gameboy-InstructionData "LD L,d" 0x6A [0] 4 (ld-r1-r2 :l :d))
   0x6B (new-gameboy-InstructionData "LD L,e" 0x6B [0] 4 (ld-r1-r2 :l :e))
   0x6C (new-gameboy-InstructionData "LD L,h" 0x6C [0] 4 (ld-r1-r2 :l :h))
   0x6D (new-gameboy-InstructionData "LD L,l" 0x6D [0] 4 (ld-r1-r2 :l :l))
   ;;;;;;;;;;;;;;;;;;;;;;;;:TODO ?;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   0x6E (new-gameboy-InstructionData "LD L,(HL)" 0x6E [0] 8 (lda8-r1-r2a :l :lh))

   0x6F (new-gameboy-InstructionData "LD L,A" 0x6F [0] 4 (ld-r1-r2 :l :a))
   ;;;;; TODO ?
   0x70 (new-gameboy-InstructionData "LD (HL),b" 0x70 [0] 8 (lda8-r1a-r2 :lh :b))
   0x71 (new-gameboy-InstructionData "LD (HL),c" 0x71 [0] 8 (lda8-r1a-r2 :lh :c))
   0x72 (new-gameboy-InstructionData "LD (HL),d" 0x72 [0] 8 (lda8-r1a-r2 :lh :d))
   0x73 (new-gameboy-InstructionData "LD (HL),e" 0x73 [0] 8 (lda8-r1a-r2 :lh :e))
   0x74 (new-gameboy-InstructionData "LD (HL),h" 0x74 [0] 8 (lda8-r1a-r2 :lh :h))
   0x75 (new-gameboy-InstructionData "LD (HL),l" 0x75 [0] 8 (lda8-r1a-r2 :lh :l))
   ;;;; TODO
   ;;0x36 (new-gameboy-InstructionData "LD (HL),n" 0x36 0 12 (ld-r1-r2 :lh :l))
   0x76 (new-gameboy-InstructionData "HALT" 0x76 [0] 4 (halt)) 

   0x77 (new-gameboy-InstructionData "LD (HL),A" 0x77 [0] 8 (lda8-r1a-r2 :lh :a))
   0x78 (new-gameboy-InstructionData "LD A,B" 0x78 [0] 4 (ld-r1-r2 :a :b))
   0x79 (new-gameboy-InstructionData "LD A,C" 0x79 [0] 4 (ld-r1-r2 :a :c))
   0x7A (new-gameboy-InstructionData "LD A,D" 0x7A [0] 4 (ld-r1-r2 :a :d))
   0x7B (new-gameboy-InstructionData "LD A,E" 0x7B [0] 4 (ld-r1-r2 :a :e))
   0x7C (new-gameboy-InstructionData "LD A,H" 0x7C [0] 4 (ld-r1-r2 :a :h))
   0x7D (new-gameboy-InstructionData "LD A,L" 0x7D [0] 4 (ld-r1-r2 :a :l))
   ;;TODO
   0x7E (new-gameboy-InstructionData "LD A,(HL)" 0x7E [0] 8 (lda-r1-r2a :a :lh))
  
   0x7F (new-gameboy-InstructionData "LD A,A" 0x7F [0] 4 (ld-r1-r2 :a :a))
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;
   ;; ADD r1 r2
   ;;
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   0x80 (new-gameboy-InstructionData "ADD A,B" 0x80 [0] 4 (add-r1-r2 :a :b))
   ;;0x7F (new-gameboy-InstructionData "LD A,A" 0x7F [0] 4 (ld-r1-r2 :a :a))
   ;;0x7F (new-gameboy-InstructionData "LD A,A" 0x7F [0] 4 (ld-r1-r2 :a :a))
   ;;0x7F (new-gameboy-InstructionData "LD A,A" 0x7F [0] 4 (ld-r1-r2 :a :a)) 
   ;;0x7F (new-gameboy-InstructionData "LD A,A" 0x7F [0] 4 (ld-r1-r2 :a :a))
   
   }))
