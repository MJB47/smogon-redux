
(in-ns 'smogon.dex)

(defpokemon :shuppet
  :name "Shuppet"
  :introduced-in :rs
  :types [[:ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:insomnia]
              :dp [:frisk :insomnia]
              :bw [:cursed-body :frisk :insomnia]]
  :stats [[44 75 35 63 33 45]]
  :weight [2.3]
  :height [0.6])

(defpokemon :banette
  :name "Banette"
  :introduced-in :rs
  :types [[:ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:insomnia]
              :dp [:frisk :insomnia]
              :bw [:cursed-body :frisk :insomnia]]
  :stats [[64 115 65 83 63 65]]
  :weight [12.5]
  :height [1.1])

(deffamily (familychain :shuppet :banette))

(deflearnset :rs
  :shuppet [:astonish :attract :body-slam :calm-mind :curse :destiny-bond :disable :double-edge :double-team :dream-eater :endure :facade :faint-attack :flash :foresight :frustration :grudge :hidden-power :icy-wind :imprison :knock-off :mimic :night-shade :nightmare :protect :psych-up :psychic :rain-dance :rest :return :screech :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :spite :substitute :sunny-day :swagger :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :will-o-wisp :body-slam :double-edge :icy-wind :mimic :nightmare]
  :banette [:hyper-beam :metronome :mud-slap])

(deflearnset :dp
  :shuppet [:astonish :attract :calm-mind :captivate :charge-beam :confuse-ray :curse :dark-pulse :destiny-bond :disable :double-team :dream-eater :embargo :endure :facade :faint-attack :flash :foresight :frustration :grudge :headbutt :hidden-power :icy-wind :imprison :knock-off :magic-coat :natural-gift :night-shade :ominous-wind :pain-split :payback :protect :psych-up :psychic :pursuit :rain-dance :rest :return :role-play :screech :secret-power :shadow-ball :shadow-sneak :shock-wave :skill-swap :sleep-talk :snatch :spite :substitute :sucker-punch :sunny-day :swagger :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :trick :trick-room :will-o-wisp :body-slam :double-edge :icy-wind :mimic :nightmare]
  :banette [:fling :giga-impact :hyper-beam :mud-slap :shadow-claw])

(deflearnset :bw
  :shuppet [:astonish :attract :calm-mind :charge-beam :confuse-ray :curse :dark-pulse :destiny-bond :disable :double-team :dream-eater :embargo :facade :faint-attack :flash :foresight :foul-play :frustration :grudge :gunk-shot :hex :hidden-power :icy-wind :imprison :knock-off :magic-coat :magic-room :night-shade :ominous-wind :pain-split :payback :protect :psych-up :psychic :pursuit :rain-dance :rest :return :role-play :round :screech :shadow-ball :shadow-sneak :skill-swap :sleep-talk :snatch :spite :substitute :sucker-punch :sunny-day :swagger :taunt :telekinesis :thief :thunder :thunder-wave :thunderbolt :torment :toxic :trick :trick-room :will-o-wisp :body-slam :double-edge :icy-wind :mimic :nightmare]
  :banette [:fling :giga-impact :hyper-beam :shadow-claw :cotton-guard])
