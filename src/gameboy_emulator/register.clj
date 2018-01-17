(ns emulator.register)
(require '[clojure.spec.alpha :as s])
(require '[emulator.common-lens :refer :all])
(require '[emulator.bytes :refer :all])
(require '[hask-tools.lens :refer :all])
(require '[hask-tools.util :refer :all])
(require '[hask-tools.debug :refer :all])
;;;;;;;;;;;;;;;;;;;;;;;;;; Registers ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Record Interpretations ;;;;;;;;;;;;;
;;;;;  Register
(defrecord Register [name word])
(make-lenses 'Register)
(try-gen-field-protocol 'register)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BiRegister8
(defrecord BiRegister8 [rega regb])
(make-lenses 'BiRegister8)
(try-gen-field-protocol 'biRegister8)
(defn ->Register->BiRegister8 [-name word]
  (get-biRegister8 (->Register -name word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Registers
(defrecord Registers [registers-map])
(make-lenses 'Registers)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Lenses ;;;;;;;;;;;;;;;;;;;;
(def lregister (rlens get-register set-register))
(def lbiRegister8 (rlens get-biRegister8 set-biRegister8))

(def lreg-hi-word
  (l-comp lbiRegister8 lregb lword))
(def lreg-lo-word
  (l-comp lbiRegister8 lrega lword8))

;;;;;;;;;;;;;;  Constructors

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Globals ;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Typeclasses ;;;;;;;;;;;;;;;;;;;;;;
;;;; Register
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(extend-type Register
  HasRegister
  (get-register [x] x)
  (set-register [whole part]
    part)

  HasBiRegister8
  (get-biRegister8 [x] 
    (let [-name   (:name x)
          word    (:word x)
          fb-name (keyword (subs (name -name) 0 1))
          sb-name  (if (= 2 (count (name -name)))
                     (keyword (subs (name -name) 1 2))
                     "")
          [fb-val,sb-val] (word16-to-word8-pair word)
          
          fb-reg  (->Register fb-name fb-val)
          sb-reg  (->Register sb-name sb-val)]
      (->BiRegister8 fb-reg sb-reg)))

  (set-biRegister8 [whole part]
    (let [ new-reg (get-register part)]
      new-reg
      ))
  
  HasWord16
  (get-word16 [x] (bit-and (:word x) (bitmask 1 16)))
  (set-word16 [x word16]
    { :pre [ (s/valid? :emulator.bytes/word16 word16)]}
    (update x :word #(l-set (lbitmask 1 16) word16 %)))

  HasWord8
  (get-word8 [x] (bit-and (:word x) (bitmask 1 8)))
  (set-word8 [x word8]
    { :pre [ (s/valid? :emulator.bytes/word8 word8) ]}
    (update x :word #(l-set (lbitmask 1 8) word8 %))))
;;;;;;;;;;;;;;;;;;;;  Register typeclasses ;;;;;;;;;;;;;;;;;;;;
;; BiRegister8
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(extend-type BiRegister8
  HasBiRegister8
  (get-biRegister8 [x] x)
  (set-biRegister8 [whole part] part)

  HasRegister
  (get-register [x]
    (let [reg-name-a (name (get-in x [:rega :name]))
          reg-name-b (name (get-in x [:regb :name]))
          reg-name   (keyword (str reg-name-a reg-name-b))
          
          word16     (l-get (l-comp
                             lword8-pair
                             lword16-from-word8-pair)
                            x)]
      (->Register reg-name word16)))
  ;;
  (set-register [whole part]
    { :pre [ (= 2 (count (name (:name part))))]}
    (dlet [ regval   (l-get lword8-pair-from-hasWord16
                           part)
          -name    (name (:name part))
          -namea   (keyword (subs -name 0 1))
          -nameb   (if (= 2 (count (name (:name part))))
                     (keyword (subs -name 1 2)))

          reg      ((comp
                    (l-set (l-comp lrega lname)
                           -namea)
                    (l-set (l-comp lregb lname)
                            -nameb)
                    (l-set lword8-pair
                           regval))
                          whole)
          ]
          reg))

  HasWord8Pair
  (get-word8-pair [x]
    [(get-in x [:rega :word]) (get-in x [:regb :word])])
  (set-word8-pair [whole [a b]]
    (-> whole (assoc-in [:rega :word] a) (assoc-in [:regb :word] b))))

(set-register (->BiRegister8 (->Register :a 4) (->Register :b 4))
              (->Register :ab 0))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameboy register map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def gameboy-reg-map
  { :fa (->Register->BiRegister8 :fa 0)
   :cb (->Register->BiRegister8 :cb 0)
   :ed (->Register->BiRegister8 :ed 0)
   :lh (->Register->BiRegister8 :lh 0)
   :sp (->Register :sp 0)
   :pc (->Register :pc 0) })
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gameboy register names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def gameboy-reg-names
  [ :a :f :fa :c :b :cb :e :d :ed :l :h :lh :sp :pc])
(defenum ::gameboy-reg-name
  :a :f :fa
  :c :b :cb
  :e :d :ed
  :l :h :lh
  :sp
  :pc)
(defn gb-reg-name-to-bireg-name [-name]
  (-name {:a :fa
          :f :fa
         :fa :fa

          :c :cb
          :b :cb
          :cb :cb
          
          :e :ed
          :d :ed
          :ed :ed
          :l :lh
          :h :lh
          :lh :lh

          :sp :sp
          :pc :pc }))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Register Flags
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def gameboy-flag-names
  [ :z :n :h :c ])
(defenum ::gameboy-flag-names
  :z :n :h :c)

(s/def ::flag-action-val #(some #{%} [:default :set :reset :nothing]))
(s/def ::flag-action (s/tuple #(= % :flag-action) ::flag-action-val)) ;;<- TODO erase?

(s/def ::spec-flag-action (s/tuple #(= % :spec-flag-action)
                                   ::gameboy-flag-names
                                   
                                   ::flag-action-val))
(s/def ::flag-actions (s/and
                       (s/map-of keyword? ::flag-action-val)
                       #(and (contains? % :z)
                             (contains? % :n)
                             (contains? % :h)
                             (contains? % :c))))
(s/valid? ::flag-actions { :z :set
                           :n  :reset
                           :h  :set
                          :c :nothing})
(defn new-flag-actions [& {:keys [z n h c]
                           :or   {z :nothing
                                  n :nothing
                                  h :nothing
                                  c :nothing}}]
  { :post [ (s/valid? ::flag-actions %) ]}
  { :z z :n n :h h :c c })

;;;;;;;;
(sdefn new-spec-flag-action
            [(::gameboy-flag-names flag)
             (::flag-action-val action)]
         
            [:spec-flag-action flag action])
(s/valid? ::flag-action [:flag-action :set])
(s/valid? ::spec-flag-action (new-spec-flag-action
                              :z
                              :set))
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /Record flags
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
