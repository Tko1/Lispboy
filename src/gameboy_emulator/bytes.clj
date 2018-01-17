(ns emulator.bytes)


(require '[hask-tools.debug :refer :all])

(require '[clojure.spec.alpha :as s])
(require '[hask-tools.util :refer :all])
(require '[hask-tools.lens :refer :all])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word8, Word16
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wordn-pred [n]
  #(and (>= % 0) (<= % (bitmask 1 n))))
;; Spec
(s/def ::word8 (wordn-pred 8))
(s/def ::word16 (wordn-pred 16))

(try-gen-field-protocol 'word16)
(try-gen-field-protocol 'word8)
;; Lens
(def lword8 (rlens get-word8 set-word8))
(def lword16 (rlens get-word16 set-word16))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bitmask lens, bit lens
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn lbitmask [n m]
  (rlens
   #(dlet [
           [-n -m] (if (< m n) [m n] [n m])
           --n     (if (< (- -n 1) 0) 0 (- -n 1))]
          (bit-shift-right (bit-and % (bitmask --n -m)) --n))
   #(dlet [
           [-n -m] (if (< m n) [m n] [n m])
           --n     (if (< (- -n 1) 0) 0 (- -n 1))
           len     (- m --n)
           max-val (bitmask 1 (- m n -1))
           v       %
           v2      (if (> %2 max-val) max-val %2) ;;part
           
           shifted (bit-shift-left v2 --n)
           shifted-cut-off (bit-and shifted (bitmask n m))

           bits-already (bit-and v (bitmask n m))
           
           empty-whole  (- v bits-already)
           ]
          (bit-or empty-whole shifted-cut-off))))
(l-set (lbitmask 5 8) 0 255)
(l-get (lbitmask 5 8) 255)
(l-set (lbitmask 7 7) 0 0xFF)  
(defn lbit [n]
  (lbitmask n n))
(l-set (lbit 3) 1 4)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word8 cont
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn validate-word8 [ word8 ]
  (cond
    (< word8 0)
    0

    (> word8 (bitmask 1 8))
    (bitmask 1 8)

    :else
    word8))

;;;; Extend long to have Word8, Word16

(extend-type Long
  HasWord16
  (get-word16 [x] (bit-and x (bitmask 1 16)))
  (set-word16 [x word16]
    { :pre [ (s/valid? :emulator.bytes/word16 word16)]}
    (l-set (lbitmask 1 16) word16 x))

  HasWord8
  (get-word8 [x] (bit-and x (bitmask 1 8)))
  (set-word8 [x word8]
    { :pre [ (s/valid? :emulator.bytes/word8 word8)]}
    (l-set (lbitmask 1 8) word8 x)))
(s/valid? :emulator.bytes/word8 100)
(set-word8 12 24)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert between word8 pairs and word16s
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def lword16-from-word8-pair 
  (rlens word8-pair-to-word16 #(word16-to-word8-pair %2)))
(def lword8-pair-from-word16
  (rlens word16-to-word8-pair #(word8-pair-to-word16 %2)))

(def lword8-pair-from-hasWord16 (l-comp  lword16 lword8-pair-from-word16))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word8 pair
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defprotocol HasWord8Pair
  (get-word8-pair [x])
  (set-word8-pair [whole part]))
(def lword8-pair (rlens get-word8-pair set-word8-pair))

;;;;;;;; Extend types for word8 pair
(extend-protocol HasWord8Pair
  Long
  (get-word8-pair [x]
    [(bit-or x (bitmask 1 8)) 
     (bit-or x (bitmask 9 16))])
  (set-word8-pair [whole part]
    (bit-or whole (word8-pair-to-word16 part))))

  
(s/def ::nibble #(and (>= % 0) (<= % (bitmask 1 4))))

(defn nibble-pair-to-word8 [[a b]]
  { :pre [(s/valid? ::nibble a)
          (s/valid? ::nibble b)]
   :post [(s/valid? :hask-tools.bytes/word8 %)]}
  (bit-or (bit-and 0x0f a) (bit-shift-left (bit-and 0x0f b) 4)))
(defn word8-to-nibble-pair [word8]
  { :pre  [(s/valid? :hask-tools.bytes/word8 word8)]
   :post [(s/valid? (s/tuple ::byte ::byte) %)]}
  [(bit-and 0xf word8) (bit-shift-right (bit-and 0xf0 word8) 4)])
 
