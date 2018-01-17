(ns emulator.common-lens)
(require '[hask-tools.lens :refer :all])
(defrecord CommonLensDummy
    [name])
(make-lenses 'CommonLensDummy)
