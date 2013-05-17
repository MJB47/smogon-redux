
(in-ns 'smogon.dex)


(defpokemon :duskull
  :name "Duskull"
  :introduced-in :rs
  :types [[:ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[20 40 90 30 90 25]]
  :weight [15]
  :height [0.8])

(defpokemon :dusclops
  :name "Dusclops"
  :introduced-in :rs
  :types [[:ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:pressure]
              :bw [:pressure]]
  :stats [[40 70 130 60 130 25]]
  :weight [30.6]
  :height [1.6])

(defpokemon :dusknoir
  :name "Dusknoir"
  :introduced-in :dp
  :types [[:ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:pressure]]
  :stats [[45 100 135 65 135 45]]
  :weight [106.6]
  :height [2.2])

(deffamily (familychain :duskull :dusclops :dusknoir))

(deflearnset :rs
  :duskull [:astonish :attract :blizzard :body-slam :calm-mind :confuse-ray :curse :destiny-bond :disable :double-edge :double-team :dream-eater :endure :facade :faint-attack :flash :foresight :frustration :future-sight :grudge :hidden-power :ice-beam :icy-wind :imprison :leer :mean-look :memento :mimic :night-shade :nightmare :pain-split :protect :psych-up :psychic :pursuit :rain-dance :rest :return :secret-power :shadow-ball :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :taunt :thief :torment :toxic :will-o-wisp :body-slam :double-edge :helping-hand :icy-wind :mimic :nightmare]
  :dusclops [:bind :counter :dynamicpunch :earthquake :fire-punch :focus-punch :hyper-beam :ice-punch :mega-kick :mega-punch :metronome :mud-slap :rock-slide :rock-smash :rock-tomb :seismic-toss :shadow-punch :strength :thunderpunch :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss])

(deflearnset :dp
  :duskull [:astonish :attract :blizzard :calm-mind :captivate :charge-beam :confuse-ray :curse :dark-pulse :destiny-bond :disable :double-team :dream-eater :embargo :endure :facade :faint-attack :flash :fling :foresight :frustration :future-sight :gravity :grudge :headbutt :hidden-power :ice-beam :icy-wind :imprison :leer :mean-look :memento :natural-gift :night-shade :ominous-wind :pain-split :payback :protect :psych-up :psychic :pursuit :rain-dance :rest :return :secret-power :shadow-ball :shadow-sneak :skill-swap :sleep-talk :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :thief :torment :toxic :trick :trick-room :will-o-wisp :body-slam :double-edge :helping-hand :icy-wind :mimic :nightmare]
  :dusclops [:bind :brick-break :earthquake :fire-punch :focus-punch :giga-impact :hyper-beam :ice-punch :mud-slap :rock-slide :rock-smash :rock-tomb :shadow-punch :strength :thunderpunch :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss]
  :dusknoir [:focus-blast])

(deflearnset :bw
  :duskull [:astonish :attract :blizzard :calm-mind :charge-beam :confuse-ray :curse :dark-pulse :destiny-bond :disable :double-team :dream-eater :embargo :facade :faint-attack :flash :fling :foresight :frustration :future-sight :gravity :grudge :hex :hidden-power :ice-beam :icy-wind :imprison :leer :mean-look :memento :night-shade :ominous-wind :pain-split :payback :protect :psych-up :psychic :pursuit :rain-dance :rest :return :round :shadow-ball :shadow-sneak :skill-swap :sleep-talk :snatch :snore :spite :substitute :sunny-day :swagger :taunt :telekinesis :thief :torment :toxic :trick :trick-room :will-o-wisp :wonder-room :body-slam :double-edge :helping-hand :icy-wind :mimic :nightmare]
  :dusclops [:bind :brick-break :bulldoze :earthquake :fire-punch :giga-impact :hyper-beam :ice-punch :rock-slide :rock-smash :rock-tomb :shadow-punch :strength :thunderpunch :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss]
  :dusknoir [:focus-blast])
