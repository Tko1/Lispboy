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

(defn wordn-pred [n]
  #(and (>= % 0) (<= % (bitmask 1 n))))

(s/def ::word8 (wordn-pred 8))
(s/def ::word16 (wordn-pred 16))
