(ns lispboy.util
  (:require [clojure.spec.alpha :as s]))

(defn bitmask [n m]
  (let [
        [-n,-m]    (if (< m n)
                     [m n]
                     [n m])
        --n        (if (< (- -n 1) 0)
                     0
                     (- -n 1))
        len        (- m --n)
        nonshifted (- (bit-shift-left 1 len) 1)
        shifted    (bit-shift-left nonshifted --n)]
    shifted))

;; Creates a predicate to check if a value is a wordn, where n
;; can be, say, word8 for a 8 bit value, word16 for 16 bit..
(defn wordn-pred [n]
  #(and (>= % 0) (<= % (bitmask 1 n))))

(s/def ::word8 (wordn-pred 8))
(s/def ::word16 (wordn-pred 16))

(defn word8-pair-to-word16 [[worda wordb]]
  {:pre [(s/valid? ::word8 worda)
         (s/valid? ::word8 wordb)]
   :post [(s/valid? ::word16 %)]}
  (bit-or
   (bit-shift-left wordb 8)
   worda))
(defn word16-to-word8-pair [ word16]
  (let [ wordb (bit-shift-right (bit-and word16 (bitmask 9 16)) 8)
        worda (bit-and word16 (bitmask 0 8))]
    [worda wordb]))
