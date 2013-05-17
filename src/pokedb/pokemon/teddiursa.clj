
(in-ns 'smogon.dex)


(defpokemon :teddiursa
  :name "Teddiursa"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:pickup]
              :dp [:pickup :quick-feet]
              :bw [:honey-gather :pickup :quick-feet]]
  :stats [[60 80 50 50 50 40]]
  :weight [8.8]
  :height [0.6])

(defpokemon :ursaring
  :name "Ursaring"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:guts]
              :dp [:guts :quick-feet]
              :bw [:guts :quick-feet :unnerve]]
  :stats [[90 130 75 75 75 55]]
  :weight [125.8]
  :height [1.8])

(deffamily (familychain :teddiursa :ursaring))



(deflearnset :gs
  :teddiursa [:attract :counter :crunch :curse :cut :defense-curl :dig :double-team :dynamicpunch :earthquake :endure :faint-attack :fire-punch :focus-energy :frustration :fury-cutter :fury-swipes :headbutt :hidden-power :ice-punch :leer :lick :metal-claw :mud-slap :protect :rest :return :roar :rock-smash :rollout :scratch :seismic-toss :slash :sleep-talk :snore :strength :sunny-day :swagger :swift :take-down :thief :thrash :thunderpunch :toxic :zap-cannon]
  :ursaring [:hyper-beam])

(deflearnset :rs
  :teddiursa [:aerial-ace :attract :body-slam :brick-break :bulk-up :counter :crunch :cut :defense-curl :dig :double-edge :double-team :dynamicpunch :earthquake :endure :facade :faint-attack :fake-tears :fire-punch :focus-punch :frustration :fury-cutter :fury-swipes :hidden-power :ice-punch :leer :lick :mega-kick :mega-punch :metal-claw :metronome :mimic :mud-slap :protect :rain-dance :rest :return :roar :rock-smash :rollout :scratch :secret-power :seismic-toss :slash :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :take-down :taunt :thief :thrash :thunderpunch :torment :toxic :yawn :body-slam :mimic :refresh]
  :ursaring [:hyper-beam :rock-slide :rock-tomb :body-slam :mimic])

(deflearnset :dp
  :teddiursa [:aerial-ace :attract :belly-drum :brick-break :bulk-up :captivate :charm :close-combat :counter :covet :cross-chop :crunch :cut :dig :double-edge :double-team :earthquake :endure :facade :faint-attack :fake-tears :fire-punch :fling :focus-punch :frustration :fury-cutter :fury-swipes :gunk-shot :headbutt :hidden-power :ice-punch :last-resort :leer :lick :metal-claw :mud-slap :natural-gift :night-slash :payback :protect :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :rollout :scratch :secret-power :seed-bomb :seismic-toss :shadow-claw :slash :sleep-talk :snore :strength :substitute :sunny-day :superpower :swagger :sweet-scent :swift :swords-dance :take-down :taunt :thief :thrash :thunderpunch :torment :toxic :yawn :body-slam :mimic :refresh]
  :ursaring [:avalanche :focus-blast :giga-impact :hammer-arm :hyper-beam :low-kick :rock-climb :scary-face :stone-edge :uproar :body-slam :mimic])

(deflearnset :bw
  :teddiursa [:aerial-ace :attract :belly-drum :brick-break :bulk-up :bulldoze :charm :chip-away :close-combat :counter :covet :cross-chop :crunch :cut :dig :double-edge :double-team :earthquake :facade :faint-attack :fake-tears :fire-punch :fling :frustration :fury-swipes :gunk-shot :hidden-power :hone-claws :hyper-voice :ice-punch :last-resort :leer :lick :metal-claw :night-slash :payback :protect :rain-dance :rest :retaliate :return :roar :rock-slide :rock-smash :rock-tomb :round :scratch :seed-bomb :seismic-toss :shadow-claw :slash :sleep-talk :snore :strength :substitute :sunny-day :superpower :swagger :sweet-scent :swords-dance :take-down :taunt :thief :thrash :thunderpunch :torment :toxic :work-up :yawn :body-slam :mimic :refresh]
  :ursaring [:focus-blast :giga-impact :hammer-arm :hyper-beam :low-kick :scary-face :smack-down :stone-edge :uproar :body-slam :mimic])

