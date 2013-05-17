
(in-ns 'smogon.dex)


(defpokemon :riolu
  :name "Riolu"
  :introduced-in :dp
  :types [[:fighting]]
  :egggroups [[]]
  :abilities [[:inner-focus :prankster :steadfast]]
  :stats [[40 70 40 35 40 60]]
  :weight [20.2]
  :height [0.7])

(defpokemon :lucario
  :name "Lucario"
  :introduced-in :dp
  :types [[:fighting :steel]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:inner-focus :justified :steadfast]]
  :stats [[70 110 70 115 70 90]]
  :weight [54]
  :height [1.2])

(deffamily (familychain :riolu :lucario))

(deflearnset :dp
  :riolu [:agility :attract :bite :blaze-kick :brick-break :bulk-up :bullet-punch :captivate :copycat :counter :cross-chop :crunch :detect :dig :double-team :drain-punch :earthquake :endure :facade :feint :fling :focus-blast :focus-punch :follow-me :force-palm :foresight :frustration :fury-cutter :headbutt :helping-hand :hi-jump-kick :hidden-power :ice-punch :iron-defense :iron-tail :low-kick :magnet-rise :mind-reader :mud-slap :natural-gift :payback :poison-jab :protect :quick-attack :rain-dance :rest :return :reversal :roar :rock-slide :rock-smash :rock-tomb :role-play :screech :secret-power :shadow-claw :sky-uppercut :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :thunderpunch :toxic :vacuum-wave :zen-headbutt]
  :lucario [:aura-sphere :bone-rush :calm-mind :close-combat :dark-pulse :dragon-pulse :extremespeed :flash-cannon :giga-impact :hyper-beam :me-first :metal-claw :metal-sound :psychic :rock-climb :shadow-ball :stone-edge :water-pulse])

(deflearnset :bw
  :riolu [:agility :attract :bite :blaze-kick :brick-break :bulk-up :bulldoze :bullet-punch :circle-throw :copycat :counter :cross-chop :crunch :detect :dig :double-team :drain-punch :dual-chop :earthquake :endure :facade :feint :final-gambit :fling :focus-blast :follow-me :force-palm :foresight :frustration :helping-hand :hi-jump-kick :hidden-power :ice-punch :iron-defense :iron-tail :low-kick :low-sweep :magnet-rise :mind-reader :nasty-plot :payback :poison-jab :protect :quick-attack :rain-dance :rest :retaliate :return :reversal :roar :rock-slide :rock-smash :rock-tomb :role-play :round :screech :shadow-claw :sky-uppercut :sleep-talk :snore :strength :substitute :sunny-day :swagger :swords-dance :thunderpunch :toxic :vacuum-wave :work-up :zen-headbutt]
  :lucario [:aura-sphere :bone-rush :calm-mind :close-combat :dark-pulse :dragon-pulse :extremespeed :flash-cannon :giga-impact :heal-pulse :hone-claws :hyper-beam :me-first :metal-claw :metal-sound :psychic :quick-guard :shadow-ball :stone-edge])
