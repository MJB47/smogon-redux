
(in-ns 'smogon.dex)


(defpokemon :staryu
  :name "Staryu"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-3]]
  :abilities [[:illuminate :natural-cure]
              :bw [:analytic :illuminate :natural-cure]]
  :stats [[30 45 55 70 85]
          :gs [30 45 55 70 55 85]]
  :weight [34.5]
  :height [0.8])

(defpokemon :starmie
  :name "Starmie"
  :introduced-in :rb
  :types [[:psychic :water]]
  :egggroups [[:water-3]]
  :abilities [[:illuminate :natural-cure]
              :bw [:analytic :illuminate :natural-cure]]
  :stats [[60 75 85 100 115]
          :gs [60 75 85 100 85 115]]
  :weight [80]
  :height [1.1])

(deffamily (familychain :staryu :starmie))



(deflearnset :rb
  :staryu [:bide :blizzard :bubblebeam :double-edge :double-team :flash :harden :hydro-pump :ice-beam :light-screen :mimic :minimize :psychic :psywave :rage :recover :reflect :rest :skull-bash :substitute :surf :swift :tackle :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic :tri-attack :water-gun]
  :starmie [:hyper-beam])

(deflearnset :gs
  :staryu [:attract :aurora-beam :barrier :blizzard :bubblebeam :curse :double-team :endure :flash :frustration :harden :hidden-power :hydro-pump :ice-beam :icy-wind :light-screen :minimize :protect :psych-up :psychic :rain-dance :rapid-spin :recover :rest :return :sleep-talk :snore :supersonic :surf :swagger :swift :tackle :thunder :thunderbolt :toxic :water-gun :waterfall :whirlpool :zap-cannon]
  :starmie [:confuse-ray :dream-eater :hyper-beam :nightmare])

(deflearnset :rs
  :staryu [:blizzard :bubblebeam :camouflage :cosmic-power :dive :double-edge :double-team :endure :facade :flash :frustration :hail :harden :hidden-power :hydro-pump :ice-beam :icy-wind :light-screen :mimic :minimize :protect :psych-up :psychic :rain-dance :rapid-spin :recover :reflect :rest :return :secret-power :sleep-talk :snore :substitute :surf :swagger :swift :tackle :thunder :thunder-wave :thunderbolt :toxic :water-gun :water-pulse :waterfall :double-edge :icy-wind :mimic]
  :starmie [:confuse-ray :dream-eater :hyper-beam :nightmare :skill-swap :double-edge :icy-wind :mimic :nightmare :refresh])

(deflearnset :dp
  :staryu [:blizzard :brine :bubblebeam :camouflage :cosmic-power :dive :double-team :endure :facade :flash :flash-cannon :frustration :gravity :gyro-ball :hail :harden :hidden-power :hydro-pump :ice-beam :icy-wind :light-screen :magic-coat :minimize :natural-gift :pain-split :power-gem :protect :psych-up :psychic :rain-dance :rapid-spin :recover :recycle :reflect :rest :return :rollout :secret-power :signal-beam :sleep-talk :snore :substitute :surf :swagger :swift :tackle :thunder :thunder-wave :thunderbolt :toxic :twister :water-gun :water-pulse :waterfall :whirlpool :double-edge :icy-wind :mimic]
  :starmie [:avalanche :confuse-ray :dream-eater :giga-impact :grass-knot :hyper-beam :skill-swap :trick :trick-room :double-edge :icy-wind :mimic :nightmare :refresh])

(deflearnset :bw
  :staryu [:blizzard :brine :bubblebeam :camouflage :cosmic-power :dive :double-team :facade :flash :flash-cannon :frustration :gravity :gyro-ball :hail :harden :hidden-power :hydro-pump :ice-beam :icy-wind :light-screen :magic-coat :minimize :pain-split :power-gem :protect :psych-up :psychic :rain-dance :rapid-spin :recover :recycle :reflect :reflect-type :rest :return :round :scald :signal-beam :sleep-talk :snore :substitute :surf :swagger :swift :tackle :thunder :thunder-wave :thunderbolt :toxic :water-gun :waterfall :double-edge :icy-wind :mimic]
  :starmie [:confuse-ray :dream-eater :giga-impact :grass-knot :hyper-beam :psyshock :skill-swap :telekinesis :trick :trick-room :wonder-room :double-edge :icy-wind :mimic :nightmare :refresh])

