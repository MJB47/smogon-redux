
(in-ns 'smogon.dex)


(defpokemon :mankey
  :name "Mankey"
  :introduced-in :rb
  :types [[:fighting]]
  :egggroups [[:ground]]
  :abilities [[:vital-spirit]
              :dp [:anger-point :vital-spirit]
              :bw [:anger-point :defiant :vital-spirit]]
  :stats [[40 80 35 35 70]
          :gs [40 80 35 35 45 70]]
  :weight [28]
  :height [0.5])

(defpokemon :primeape
  :name "Primeape"
  :introduced-in :rb
  :types [[:fighting]]
  :egggroups [[:ground]]
  :abilities [[:vital-spirit]
              :dp [:anger-point :vital-spirit]
              :bw [:anger-point :defiant :vital-spirit]]
  :stats [[65 105 60 60 95]
          :gs [65 105 60 60 70 95]]
  :weight [32]
  :height [1])

(deffamily (familychain :mankey :primeape))

(deflearnset :rb
  :mankey [:bide :body-slam :counter :dig :double-edge :double-team :focus-energy :fury-swipes :karate-chop :leer :low-kick :mega-kick :mega-punch :metronome :mimic :pay-day :rage :rest :rock-slide :scratch :screech :seismic-toss :skull-bash :strength :submission :substitute :swift :take-down :thrash :thunder :thunderbolt :toxic]
  :primeape [:hyper-beam])

(deflearnset :gs
  :mankey [:attract :beat-up :counter :cross-chop :curse :defense-curl :detect :dig :double-team :dynamicpunch :endure :fire-punch :focus-energy :foresight :frustration :fury-swipes :headbutt :hidden-power :ice-punch :iron-tail :karate-chop :leer :low-kick :meditate :mud-slap :protect :psych-up :rest :return :reversal :rock-slide :rock-smash :scratch :screech :seismic-toss :sleep-talk :snore :strength :sunny-day :swagger :swift :thief :thrash :thunder :thunderbolt :thunderpunch :toxic]
  :primeape [:hyper-beam :rage])

(deflearnset :rs
  :mankey [:aerial-ace :attract :beat-up :body-slam :brick-break :bulk-up :counter :cross-chop :defense-curl :dig :double-edge :double-team :dynamicpunch :earthquake :endure :facade :fire-punch :focus-energy :focus-punch :foresight :frustration :fury-swipes :hidden-power :ice-punch :iron-tail :karate-chop :leer :low-kick :meditate :mega-kick :mega-punch :metronome :mimic :mud-slap :overheat :protect :psych-up :rain-dance :rest :return :revenge :reversal :rock-slide :rock-smash :rock-tomb :scratch :screech :secret-power :seismic-toss :sleep-talk :smellingsalt :snore :strength :substitute :sunny-day :swagger :swift :taunt :thief :thrash :thunder :thunderbolt :thunderpunch :toxic :body-slam :double-edge :mimic]
  :primeape [:hyper-beam :rage :body-slam :double-edge :helping-hand :mimic])

(deflearnset :dp
  :mankey [:aerial-ace :assurance :attract :beat-up :brick-break :bulk-up :captivate :close-combat :counter :covet :cross-chop :dig :double-team :earthquake :encore :endeavor :endure :facade :fire-punch :fling :focus-blast :focus-energy :focus-punch :foresight :frustration :fury-swipes :gunk-shot :headbutt :helping-hand :hidden-power :ice-punch :iron-tail :karate-chop :leer :low-kick :meditate :mud-slap :natural-gift :outrage :overheat :payback :poison-jab :protect :punishment :rain-dance :rest :return :revenge :reversal :rock-climb :rock-slide :rock-smash :rock-tomb :role-play :scratch :screech :secret-power :seed-bomb :seismic-toss :sleep-talk :smellingsalt :spite :strength :substitute :sunny-day :swagger :swift :taunt :thief :thrash :thunder :thunderbolt :thunderpunch :toxic :u-turn :uproar :vacuum-wave :body-slam :double-edge :mimic]
  :primeape [:giga-impact :hyper-beam :rage :stone-edge :body-slam :double-edge :helping-hand :mimic])

(deflearnset :bw
  :mankey [:acrobatics :aerial-ace :assurance :attract :beat-up :brick-break :bulk-up :bulldoze :close-combat :counter :covet :cross-chop :dig :double-team :dual-chop :earthquake :encore :endeavor :facade :final-gambit :fire-punch :fling :focus-blast :focus-energy :focus-punch :foresight :frustration :fury-swipes :gunk-shot :helping-hand :hidden-power :hone-claws :ice-punch :iron-tail :karate-chop :leer :low-kick :low-sweep :meditate :outrage :overheat :payback :poison-jab :protect :punishment :rain-dance :rest :retaliate :return :revenge :reversal :rock-slide :rock-smash :rock-tomb :role-play :round :scratch :screech :seed-bomb :seismic-toss :sleep-talk :smack-down :smellingsalt :spite :strength :substitute :sunny-day :swagger :taunt :thief :thrash :thunder :thunderbolt :thunderpunch :toxic :u-turn :uproar :work-up :body-slam :double-edge :mimic]
  :primeape [:giga-impact :hyper-beam :rage :stone-edge :body-slam :double-edge :helping-hand :mimic])
