(ns emulator.cpu
  (:gen-class))
(require '[hask-tools.debug :refer :all])
(require '[hask-tools.util :refer :all])
(require '[hask-tools.lens :refer :all])

(require '[hask-tools.monad.core :refer :all])
(require '[hask-tools.monad.maybe :refer :all])
(require '[hask-tools.monad.parser :refer :all])
(require '[hask-tools.monad.identity :refer :all])

(require '[clojure.spec.alpha :as s])

(require '[emulator.common-lens :refer :all])

(require '[emulator.instruction :refer :all])
(require '[emulator.cartridge :refer :all])
(require '[emulator.bytes :refer :all])
(require '[emulator.register :refer :all])

(defrecord Cpu [ registers memory-map instruction-dataset])
(make-lenses 'Cpu)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;; Memory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Records

(defrecord MemoryMap [ memory ])
(make-lenses 'MemoryMap)

(def gameboy-memory-size 0x10000)
;;;;;;;;;;;;;;;;;;;;;;; Specs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(s/def ::gameboy-addr
  #(and (integer? %) (>= % 0) (< % gameboy-memory-size)))
;;;;;;;;;;
(s/def ::gameboy-memory
  (s/and
   #(vector? %)
   #(= gameboy-memory-size (count %))
   (s/coll-of :emulator.bytes/word8)))
;;Tests
(s/valid? (s/and (s/coll-of :emulator.bytes/word8) #(vector? %)) [0 0 0])
(s/valid? ::gameboy-memory (vec (map (fn [_] (short 0)) (range gameboy-memory-size))))
;;/Tests
;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;; Globals;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lmm-interrupt-enable-register (lrange 0xFFFF 0xFFFF))
(def lmm-internal-ram              (lrange 0xFF80 0xFFFE))
(def lmm-empty-nio1                (lrange 0xFF4C 0xFF79)) ;;FIX
(def lmm-iop                       (lrange 0xFF00 0xFF4B)) ;;FIX
(def lmm-empty-nio2                (lrange 0xFEA0 0xFEFF)) ;;FIX
(def lmm-oam                       (lrange 0xFE00 0xFE9F)) ;;FIX
(def lmm-8kB-internal-ram-echo      (lrange 0xE000 0xFDFF)) ;;FIX
(def lmm-8kB-internal-ram           (lrange 0xE000 0xC000))
(def lmm-8kB-switchable-ram-bank    (lrange 0 0))
(def lmm-8kB-vram                   (lrange 0x8000 0x9FFF))
(def lmm-vram                       (lrange 0 0))
(def lmm-16kB-switchable-ram-bank (lrange 0x4000 0x7999))
(def lmm-16kB-rom-bank-0                  (lrange 0 0x3999))
(def lmm-cartridge                  (lrange 0 0x7999))

(def default-gameboy-memory
  (vec (map (fn [_] (short 0)) (range gameboy-memory-size))))
(assert (s/valid? ::gameboy-memory default-gameboy-memory))

;;;;;;;;;;;;;;;;;;;; Global instances ;;;;;;;;;;;;;;;;;;;;;;
(def gameboy-cpu
  (->Cpu
   (->Registers
    gameboy-reg-map)
   default-gameboy-memory
   gameboy-instruction-set))
;;;;;;;;;;;;;;;;;;;;;;  Lens ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level
;; Access byte at addr
(defn lmm-addr-byte [ addr ]
  { :pre [ (s/valid? ::gameboy-addr addr)]}
  (lat addr))
;; Access word8 at addr
(defn lmm-addr-word8 [ addr ]
  { :pre [ (s/valid? ::gameboy-addr addr)]}
  (lat addr))
;; Access word16 at addr 
(defn lmm-addr-word16 [ addr ]
  (rlens #(word8-pair-to-word16
           [(l-get (lmm-addr-word8 addr) %)
            (l-get (lmm-addr-word8 (+ 1 addr)) %)])
         #(as-> % mm
            (l-set (lmm-addr-word8 addr)
                   (first (word16-to-word8-pair %2)) mm)
            (l-set (lmm-addr-word8 (+ 1 addr))
                   (second (word16-to-word8-pair %2)) mm))))


;; High level?
(defn lcpu-reg [ reg-name ]
  { :pre [ (some #{reg-name} gameboy-reg-names) ] }
  (l-comp lregisters
          lregisters-map
          (if (= 2 (count (name reg-name)))
            (lens-map reg-name)
            ;;else
            ;; TODO generalize tihs
            (l-comp
             (lens-map (gb-reg-name-to-bireg-name reg-name)) 
             (if (some #{reg-name}
                       [:f :c :e :l])
               lrega
               lregb))
            )))
(defn lcpu-flag [ flag-name ]
  { :pre [ (some #{ flag-name } gameboy-flag-names )]}
  (l-comp (lcpu-reg :f)
          lword
          (flag-name
           { :z
            (lbit 8)
            :n
            (lbit 7)
            :h
            (lbit 6)
            :c
            (lbit 5)})))
(defn lcpu-reg-word [ reg-name ]
  (l-comp (lcpu-reg reg-name) lregister lword))
(defn lcpu-reg-word8 [ reg-name ]
  (l-comp (lcpu-reg reg-name) lregister lword8))
(defn lcpu-reg-word16 [ reg-name ]
  (l-comp (lcpu-reg reg-name) lregister lword16))

(defn over-reg-cpu [cpu reg-name fn]
  (l-over (lcpu-reg-word reg-name) fn cpu))
(defn over-reg8-cpu [cpu reg-name fn]
  (l-over (lcpu-reg-word8 reg-name) fn cpu))
(defn over-reg16-cpu [cpu reg-name fn]
  (l-over (lcpu-reg-word16 reg-name) fn cpu))

(defn write-reg-hi-cpu [ cpu reg-name val]
  (l-set (l-comp (lcpu-reg reg-name) lreg-hi-word) val cpu))

(write-reg-hi-cpu gameboy-cpu :a 4)
(defn write-reg-lo-cpu [ cpu reg-name val ]
  (l-set (l-comp (lcpu-reg reg-name) lreg-lo-word) val cpu))

(defn move-r1-r2-cpu [ cpu r1 r2 ]
  (l-set (lcpu-reg-word r2)
         (l-get r1 cpu)
         cpu)) 
(defn write-reg-cpu [ cpu reg-name val ]
  (l-set (lcpu-reg-word reg-name) val cpu))
(defn write-reg8-cpu [ cpu reg-name val ]
  (l-set (lcpu-reg-word8 reg-name) val cpu))
(defn write-reg16-cpu [ cpu reg-name val ]
  (l-set (lcpu-reg-word16 reg-name) val cpu))

(defn laddr8 [ addr ]
  (l-comp lmemory-map (lmm-addr-word8 addr)))
(defn laddr16 [ addr ]
  (l-comp lmemory-map (lmm-addr-word16 addr)))

(defn write-addr8-cpu [ cpu addr val ]
  { :pre [ (s/valid? :emulator.bytes/word8 val) ] }
  (l-set (laddr8 addr) val cpu))
(defn write-addr16-cpu [ cpu addr val ]
  { :pre [ (s/valid? :emulator.bytes/word16 val) ] }
  (l-set (laddr16 addr) val cpu))


(defn over-addr8-cpu [ cpu addr fn ]
  { :pre [ (s/valid? :emulator.bytes/word8 val) ] }
  (l-over (laddr8 addr) fn cpu))
(defn over-addr16-cpu [ cpu addr fn ]
  { :pre [ (s/valid? :emulator.bytes/word16 val) ] }
  (l-over (laddr16 addr) fn cpu))
(write-reg-cpu gameboy-cpu :a 1028)
(over-reg-cpu  gameboy-cpu :a #(* (+ % 3) 2))
(defn read-reg-cpu [ cpu reg-name ]
  (l-get (lcpu-reg-word reg-name) cpu))
(-> gameboy-cpu (write-reg-cpu :fa 1028) (read-reg-cpu :f))

(defn read-addr8-cpu [cpu addr ]
  (l-get (l-comp lmemory-map (lmm-addr-word8 addr)) cpu))
(defn read-addr16-cpu [cpu addr ]
  (l-get (l-comp lmemory-map (lmm-addr-word16 addr)) cpu))
(read-addr16-cpu (write-addr16-cpu gameboy-cpu 0xfffe 0xff) 0xfffe)



(defncurry reset-flag-cpu [ flag cpu ]
  (l-set (lcpu-flag flag) 0 cpu))
(defncurry set-flag-cpu [ flag cpu ]
  (l-set (lcpu-flag flag) 1 cpu))

(def reset-n-flag-cpu (reset-flag-cpu :n)) ;;TODO
(def set-n-flag-cpu   (set-flag-cpu   :n))

;;<elisp>
;;(insert (flag-fn "z"))
(def reset-z-flag-cpu (reset-flag-cpu :z))
(def set-z-flag-cpu   (set-flag-cpu   :z))

;;<elisp>
;;(insert (flag-fn "h"))
(def reset-h-flag-cpu (reset-flag-cpu :h))
(def set-h-flag-cpu   (set-flag-cpu   :h))

;;<elisp>
;;(insert (flag-fn "c"))
(def reset-c-flag-cpu (reset-flag-cpu :c))
(def set-c-flag-cpu   (set-flag-cpu   :c))

(defn add-flag-actions [ flag-actions command ]
  (fncurry [r1 gr2 cpu instr]
    (dlet [old-val     (read-reg-cpu cpu r1)
           post-val    (read-reg-cpu (command r1 cpu instr) r1)
           diff        (- post-val old-val)
           nibble-flow? (> (+ (bit-and 0x0F old-val)
                              (bit-and 0x0F diff))
                           0x0F)
           z           (:z flag-actions)
           n           (:n flag-actions)
           h           (:h flag-actions)
           c           (:c flag-actions)]
          (as-> cpu c
            
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Half-carry-flag
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;Set
            (if (= h :set)
              (set-h-flag-cpu c)
              c)
            ;;Default
            (if (= h :default)
              (if nibble-flow?
                (set-h-flag-cpu c)
                (reset-h-flag-cpu c))
              c)
            ;;Reset
            (if (= h :reset)
              (reset-h-flag-cpu c)
              c)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Computation
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (command r1 c instr)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Zero flag
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Set?
            (if (= z :set)
              (set-z-flag-cpu c)
              c)
            ;; Default?
            (if (= z :default)
              (if (= 0 (read-reg-cpu c r1))
                (set-z-flag-cpu c)
                (reset-z-flag-cpu c))
              c)
            ;;Reset?
            (if (= z :reset)
              (reset-z-flag-cpu c)
              c)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Negative flag
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; :Set
            (if (= n :set)
              (set-n-flag-cpu c)
              c)
            ;;Reset
            (if (= n :reset)
              (reset-n-flag-cpu c)
              c)
            ;; Default
            (if (= n :default)
              c
              c)))))
(defn add-flag-actions-2 [ flag-actions command ]
  (fncurry [r1 r2 cpu instr]
    (dlet [old-val     (read-reg-cpu cpu r1)
           post-val    (read-reg-cpu (command r1 r2 cpu instr) r1)
           diff        (- post-val old-val)
           nibble-flow? (> (+ (bit-and 0x0F old-val)
                              (bit-and 0x0F diff))
                           0x0F)
           z           (:z flag-actions)
           n           (:n flag-actions)
           h           (:h flag-actions)
           c           (:c flag-actions)]
          (as-> cpu c
            
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Half-carry-flag
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;;Set
            (if (= h :set)
              (set-h-flag-cpu c)
              c)
            ;;Default
            (if (= h :default)
              (if nibble-flow?
                (set-h-flag-cpu c)
                (reset-h-flag-cpu c))
              c)
            ;;Reset
            (if (= h :reset)
              (reset-h-flag-cpu c)
              c)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Computation
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (command r1 r2 c instr)
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Zero flag
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Set?
            (if (= z :set)
              (set-z-flag-cpu c)
              c)
            ;; Default?
            (if (= z :default)
              (if (= 0 (read-reg-cpu c r1))
                (set-z-flag-cpu c)
                (reset-z-flag-cpu c))
              c)
            ;;Reset?
            (if (= z :reset)
              (reset-z-flag-cpu c)
              c)

          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; Negative flag
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; :Set
            (if (= n :set)
              (set-n-flag-cpu c)
              c)
            ;;Reset
            (if (= n :reset)
              (reset-n-flag-cpu c)
              c)
            ;; Default
            (if (= n :default)
              c
              c)))))


(defncurry ld-r1-r2 [ r1 r2 cpu instr ]
  (let [ reg-from-name r2
         reg-to-name   r1 ]
    (write-reg-cpu
     cpu
     reg-to-name
     (read-reg-cpu cpu reg-from-name))))
(as-> gameboy-cpu cpu
  (write-reg-cpu cpu :a 4)
  (write-reg-cpu cpu :f 4)
  (write-reg-cpu cpu :h 1028)
  (ld-r1-r2 :sp :fa  cpu nil)
  (ld-r1-r2 :pc :a   cpu nil)
  (ld-r1-r2 :lh :h   cpu nil))
;; 0x01 , 
(defncurry ld-n-nn [ reg-name cpu instr ]
  (let [ val (first (l-get largs instr)) ]
    (write-reg-cpu cpu reg-name val)))
;;;;;;;;;;; Ld addresses ;;;;;;;;;;;;;;;;;;;;;;
;; 0x02
(defncurry lda16-r1a-r2 [ r1 r2 cpu instr ]
  (let [reg-from r2
        reg-to   r1
        val      (read-reg-cpu cpu reg-from)
        addr     (read-reg-cpu cpu reg-to)]
    (write-addr16-cpu cpu addr val)))
;; Next time, make this part of possible register lenses

(defncurry lda16-nna-r2 [ r2 cpu instr ]
  (let [nn (first (l-get largs instr))
        val (read-reg-cpu cpu r2)]
    (write-addr16-cpu cpu nn val)))

(defncurry lda8-r1a-r2 [ r1 r2 cpu instr ]
  (let [reg-from r2
        reg-to   r1
        val      (read-reg-cpu cpu reg-from)
        addr     (read-reg-cpu cpu reg-to)]
    (write-addr8-cpu cpu addr val)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defncurry lda16-r1-r2a [ r1 r2 cpu instr ]
  (let [reg-from r2
        reg-to   r1
        addr      (read-reg-cpu cpu reg-from)
        val       (read-addr16-cpu cpu addr)]
    (write-reg-cpu cpu reg-to val)))
(defncurry lda8-r1-r2a [ r1 r2 cpu instr ]
  (let [reg-from r2
        reg-to   r1
        addr      (read-reg-cpu cpu reg-from)
        val       (read-addr8-cpu cpu addr)]
    (write-reg-cpu cpu reg-to val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def add8-r1-r2
  
  (add-flag-actions-2
   (new-flag-actions :n :reset)
   (fncurry [ r1 r2 cpu instr ]
            (let [ val (read-reg-cpu cpu r2) ]
              (as-> cpu c
                (over-reg-cpu c r1 #(+ val %))

                (if (bit-and 0xffff0000 (read-reg-cpu c r1))
                  (set-c-flag-cpu c)
                  (reset-c-flag-cpu c))

                (over-reg-cpu c r1 #(bit-and % 0xffff))

                (if (> (+ (bit-and (read-reg-cpu c r1) 0x0f)
                          (bit-and val 0x0f))
                       0x0f)
                  (set-h-flag-cpu c)
                  (reset-h-flag-cpu c)))))))

(def add16-r1-r2
  
  (add-flag-actions-2
   (new-flag-actions :n :reset)
   (fncurry [ r1 r2 cpu instr ]
            (let [ val (read-reg-cpu cpu r2) ]
              (as-> cpu c
                (over-reg-cpu c r1 #(+ val %))

                (if (bit-and 0xffff0000 (read-reg-cpu c r1))
                  (set-c-flag-cpu c)
                  (reset-c-flag-cpu c))

                (over-reg-cpu c r1 #(bit-and % 0xffff))

                (if (> (+ (bit-and (read-reg-cpu c r1) 0x0f)
                          (bit-and val 0x0f))
                       0x0f)
                  (set-h-flag-cpu c)
                  (reset-h-flag-cpu c)))))))

(-> gameboy-cpu (write-reg-cpu :f 0xff) (over-reg-cpu :f #(+ 1 %)))



;;0x03
(defncurry inc-nn [ r1 cpu instr ]
  (over-reg-cpu cpu r1 #(+ 1 %)))
;;0x04
(def inc-n
  (add-flag-actions (new-flag-actions :z :default
                                      :n :reset
                                      :h :default)
                    (fn [r1 cpu instr]
                      (over-reg-cpu cpu r1 #(+ 1 %)))))
(def inc8-n
  (add-flag-actions (new-flag-actions :z :default
                                      :n :reset
                                      :h :default)
                    (fn [r1 cpu instr]
                      (over-reg8-cpu cpu r1 #(+ 1 %)))))
;;To test
(def inca-nn
  (add-flag-actions (new-flag-actions :z :default
                                      :n :reset
                                      :h :default)
                    (fn [r1 cpu instr]
                      (let [addr    (read-reg-cpu  cpu r1)]
                        (over-addr16-cpu cpu addr #(+ % 1))))))
(def deca-nn
  (add-flag-actions (new-flag-actions :n :set
                                      :h :default)
                    (fn [r1 cpu instr]
                      (dlet [ addr (read-reg-cpu cpu r1)
                              old-val (read-addr16-cpu cpu addr)
                             half-carry? (not (bit-and old-val 0x0F))]
                            (as-> cpu c
                              (if half-carry?
                                (set-h-flag-cpu c)
                                (reset-h-flag-cpu c))
                              (over-addr16-cpu c addr #(- % 1)))))))
(comment
(as-> gameboy-cpu c
  (inc-n :l c nil)
  (write-reg-cpu c :l 0xff)
  (inc-n :l c nil)))
;;0x05
(def dec-n
  (add-flag-actions (new-flag-actions :n :set
                                      :h :default)
                    (fn [r1 cpu instr]
                      (dlet [ old-val (read-reg-cpu cpu r1)
                             half-carry? (not (bit-and old-val 0x0F))]
                            (as-> cpu c
                              (if half-carry?
                                (set-h-flag-cpu c)
                                (reset-h-flag-cpu c))
                              (over-reg-cpu c r1 #(- % 1)))))))
(defncurry dec-nn [r1 cpu instr]
  (over-reg-cpu cpu r1 #(- % 1)))
(defncurry inc-n-2 [r1 cpu instr]
  ;; Z - set if result is 0
  ;; N - reset (after ? )
  ;; H - set if carry from bit 3
  ;; C - not affected
  (dlet [old-val      (read-reg-cpu cpu r1)
         half-carry?  (= 0x0F (bit-and old-val 0x0F))]
        (as-> cpu c
          ;; Self half carry?
          (if half-carry?
            (set-h-flag-cpu c)
            (reset-h-flag-cpu c))
          ;; inc
          (over-reg-cpu c r1 #(+ 1 %))
          ;; Set zero flag?
          (if (= 0 (read-reg-cpu c r1))
            (set-z-flag-cpu c)
            (reset-z-flag-cpu c))
          ;; Clear negative
          (reset-n-flag-cpu c))))
;; 0x06,
;; 0x0E (14),
;; 0x16 (22),
;; 0x1E (30)
(defncurry ld-nn-n [ reg-name cpu instr ]
  (let [ val (first (l-get largs instr))]
    (write-reg-cpu cpu reg-name val)))

(def dec-n
  (add-flag-actions (new-flag-actions :n :set
                                      :h :default)
                    (fn [r1 cpu instr]
                      (dlet [ old-val (read-reg-cpu cpu r1)
                             half-carry? (not (bit-and old-val 0x0F))]
                            (as-> cpu c
                              (if half-carry?
                                (set-h-flag-cpu c)
                                (reset-h-flag-cpu c))
                              (over-reg-cpu c r1 #(- % 1)))))))

(def rlc
  (add-flag-actions (new-flag-actions :n :reset
                                      :z :reset
                                      :h :reset)
                    (fn [r1 cpu instr ]
                      (dlet [ carry-bit (l-get (lbit 8) (read-reg-cpu cpu r1))
                              carry-bit? (= 1 carry-bit)]
                            (as-> cpu c
                              (if carry-bit?
                                (set-c-flag-cpu c)
                                (reset-c-flag-cpu c))
                              
                            
                              (over-reg-cpu c r1 #(l-set (lbit 8) 0 %))
                              (over-reg-cpu c r1 #(bit-shift-left % 1))
                              (over-reg-cpu c r1 #(+ % carry-bit)))))))
(def rrc
  (add-flag-actions (new-flag-actions :n :reset
                                      :z :reset
                                      :h :reset)
                    (fn [r1 cpu instr ]
                      (dlet [ carry-bit (l-get (lbit 1) (read-reg-cpu cpu r1))
                              carry-bit? (= 1 carry-bit)]
                            (as-> cpu c
                              (if carry-bit?
                                (set-c-flag-cpu c)
                                (reset-c-flag-cpu c))
    
                              (over-reg-cpu c r1 #(bit-shift-right % 1))
                              (if carry-bit?
                                (over-reg-cpu c r1 #(l-set (lbit 8) carry-bit))
                                c)
                              
                              )))))
(def rrca (rrc :a))
(def rlca (rlc :a))

(defncurry stop [ cpu instr]
  cpu)
(defncurry halt [ cpu instr ]
  cpu)
(rlca gameboy-cpu nil)  

(defncurry lda8-r1a-n [ r1 cpu instr ]
  (dlet [addr (read-reg-cpu cpu r1)
         val (first (l-get largs instr))]
        (write-addr8-cpu cpu addr val)))
(defncurry lda16-r1a-n [ r1 cpu instr ]
  (dlet [addr (read-reg-cpu cpu r1)
         val (first (l-get largs instr))]
        (write-addr16-cpu cpu addr val)))
  
(defncurry lda8-na-r1 [ r1 cpu instr ]
  (dlet [ addr (first (l-get largs instr))
          -val  (read-reg-cpu cpu r1)]
        (write-addr8-cpu cpu addr -val)))

(defncurry lda16-na-r1 [ r1 cpu instr ]
  (dlet [ addr (first (l-get largs instr))
          -val  (read-reg-cpu cpu r1)]
        (write-addr16-cpu cpu addr -val)))
(as-> gameboy-cpu cpu (inc-nn :cb cpu nil) (inc-nn :a cpu nil)
      (inc-nn :l cpu nil) (inc-nn :lh cpu nil))


;;(count tetris-cartridge)
;;(l-get (lrange 0 0x7999) tetris-cartridge)



;;(count pokemon-yellow-cartridge)
;;0x8000  1 048 576 
(get-byte-at pokemon-yellow-cartridge 0x100)
(defn reset-cpu [cpu]
  (l-qexpr :set [(lcpu-reg-word16 :pc)]
           :val 0x100
           :to  cpu))
(l-qexpr :set [(lcpu-reg-word16 :pc)]
         :val 0x12
         :to gameboy-cpu)
(dprint-cpu (reset-cpu gameboy-cpu))

(defn dprint-cpu [cpu]
  (println "******************************")
  (println (l-get lregisters cpu))
  (println "******************************"))
;;(defn read-instruction )
(defn load-cartridge [cpu cart]
  {:pre [ (= 0x8000 (count cart))
         (s/valid? :emulator.cartridge/cartridge cart)
         (s/valid? (stype Cpu) cpu)]}
  (l-qexpr :set [lmemory-map lmm-cartridge]
           :val cart
           :to  cpu))
(load-cartridge gameboy-cpu tetris-cartridge)
(def tetris-gameboy-cpu (-> gameboy-cpu (load-cartridge tetris-cartridge) reset-cpu))
tetris-gameboy-cpu
(defn inc-pc [cpu]
  (l-over (lcpu-reg-word16 :pc) inc cpu))
(defn lgoto-pc-range [cpu range]
  (as-> (l-get (lcpu-reg-word16 :pc) cpu) ,, res
    ;;;;;;
    (l-comp lmemory-map lmm-cartridge (lrange res (+ res range)))))
(defn lgoto-pc-until [cpu until]
  (as-> (l-get (lcpu-reg-word16 :pc) cpu) ,, res
    ;;;;;;
    (l-comp lmemory-map lmm-cartridge (lrange res until))))
(defn lgoto-pc [cpu]
  (lgoto-pc-until cpu 0x7999))
(defn lgoto-pc-5 [cpu]
  (lgoto-pc-range cpu 5))
(defn get-pc [cpu]
  (l-qexpr :get [(lcpu-reg-word16 :pc)]
           :from cpu))
(let [ loaded-cpu (reset-cpu (load-cartridge gameboy-cpu tetris-cartridge))]
  (l-get (lgoto-pc-5 loaded-cpu) loaded-cpu))
(defn read-instruction-at-pc [cpu instr-set]
  (run-parse-res
   (cpu-instructionp cpu)
   (l-qexpr :get [(lgoto-pc-5 cpu)] :from cpu)))
(-> gameboy-cpu reset-cpu read-pc)
(defn cpu-exec-instruction [cpu instr-])
(defn cpu-step [cpu]
  ;;Finally, I've found a use for this
  (domonad-env
   ->identity
   (instr <- (read-instruction-at-pc cpu instr-set))
   (c1 <- (inc-pc cpu))
   c1))
(defn gameboy-cpu-step [cpu] (cpu-step cpu gameboy-instruction-set))
(dprint-cpu (gameboy-cpu-step tetris-gameboy-cpu))

(run-parse (read-instructionp gameboy-instruction-set) [0 128 0])
(read-at-pc tetris-gameboy-cpu gameboy-instruction-set)
(dprint-cpu (cpu-step gameboy-cpu)) 0x
(dprint-cpu (l-set (lcpu-reg-word16 :pc) 3 gameboy-cpu))
