(ns lispboy.cartridge
  (:require [lispboy.util :as util]
            [clojure.spec.alpha :as s]))

(s/def ::cartridge (s/coll-of ::util/word8))
