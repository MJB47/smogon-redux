
(in-ns 'smogon.dex)

(defpokemon :magnemite
  :name "Magnemite"
  :introduced-in :rb
  :types [[:electric :steel]]
  :egggroups [[:mineral]]
  :abilities [[:magnet-pull :sturdy]
              :bw [:analytic :magnet-pull :sturdy]]
  :stats [[25 35 70 95 45]
          :gs [25 35 70 95 55 45]]
  :weight [6]
  :height [0.3])

(defpokemon :magneton
  :name "Magneton"
  :introduced-in :rb
  :types [[:electric :steel]]
  :egggroups [[:mineral]]
  :abilities [[:magnet-pull :sturdy]
              :bw [:analytic :magnet-pull :sturdy]]
  :stats [[50 60 95 120 70]
          :gs [50 60 95 120 70 70]]
  :weight [60]
  :height [1])

(defpokemon :magnezone
  :name "Magnezone"
  :introduced-in :dp
  :types [[:electric :steel]]
  :egggroups [[:mineral]]
  :abilities [[:analytic :magnet-pull :sturdy]]
  :stats [[70 70 115 130 90 60]]
  :weight [180]
  :height [1.2])

(deffamily (familychain :magnemite :magneton :magnezone))

(deflearnset :rb
  :magnemite [:bide :double-edge :double-team :flash :mimic :rage :reflect :rest :screech :sonicboom :substitute :supersonic :swift :tackle :take-down :teleport :thunder :thunder-wave :thunderbolt :thundershock :toxic :agility]
  :magneton [:hyper-beam])

(deflearnset :gs
  :magnemite [:curse :double-team :endure :flash :frustration :hidden-power :lock-on :protect :rain-dance :rest :return :rollout :screech :sleep-talk :snore :sonicboom :supersonic :swagger :swift :tackle :thunder :thunder-wave :thunderbolt :thundershock :toxic :zap-cannon :agility]
  :magneton [:hyper-beam :tri-attack])

(deflearnset :rs
  :magnemite [:double-edge :double-team :endure :facade :flash :frustration :hidden-power :lock-on :metal-sound :mimic :protect :rain-dance :reflect :rest :return :rollout :screech :secret-power :shock-wave :sleep-talk :snore :sonicboom :spark :substitute :sunny-day :supersonic :swagger :swift :tackle :thunder :thunder-wave :thunderbolt :thundershock :toxic :zap-cannon :double-edge :mimic]
  :magneton [:hyper-beam :tri-attack :refresh :double-edge :double-edge :mimic :refresh])

(deflearnset :dp
  :magnemite [:charge-beam :discharge :double-team :endure :explosion :facade :flash :flash-cannon :frustration :gravity :gyro-ball :hidden-power :iron-defense :light-screen :lock-on :magic-coat :magnet-bomb :magnet-rise :metal-sound :mirror-shot :natural-gift :protect :psych-up :rain-dance :recycle :reflect :rest :return :rollout :screech :secret-power :shock-wave :signal-beam :sleep-talk :snore :sonicboom :spark :substitute :sunny-day :supersonic :swagger :swift :tackle :thunder :thunder-wave :thunderbolt :thundershock :toxic :zap-cannon :double-edge :mimic]
  :magneton [:giga-impact :hyper-beam :tri-attack :refresh :double-edge :double-edge :mimic :refresh]
  :magnezone [:barrier :iron-head :mirror-coat])

(deflearnset :bw
  :magnemite [:charge-beam :discharge :double-team :electro-ball :electroweb :explosion :facade :flash :flash-cannon :frustration :gravity :gyro-ball :hidden-power :iron-defense :light-screen :lock-on :magic-coat :magnet-bomb :magnet-rise :metal-sound :mirror-shot :protect :psych-up :rain-dance :recycle :reflect :rest :return :round :screech :signal-beam :sleep-talk :snore :sonicboom :spark :substitute :sunny-day :supersonic :swagger :tackle :thunder :thunder-wave :thunderbolt :thundershock :toxic :volt-switch :wild-charge :zap-cannon :double-edge :mimic]
  :magneton [:giga-impact :hyper-beam :tri-attack :refresh :double-edge :double-edge :mimic :refresh]
  :magnezone [:barrier :iron-head :mirror-coat])
