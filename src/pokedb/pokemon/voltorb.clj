
(in-ns 'smogon.dex)


(defpokemon :voltorb
  :name "Voltorb"
  :introduced-in :rb
  :types [[:electric]]
  :egggroups [[:mineral]]
  :abilities [[:soundproof :static]
              :bw [:aftermath :soundproof :static]]
  :stats [[40 30 50 55 100]
          :gs [40 30 50 55 55 100]]
  :weight [10.4]
  :height [0.5])

(defpokemon :electrode
  :name "Electrode"
  :introduced-in :rb
  :types [[:electric]]
  :egggroups [[:mineral]]
  :abilities [[:soundproof :static]
              :bw [:aftermath :soundproof :static]]
  :stats [[60 50 70 80 140]
          :gs [60 50 70 80 80 140]]
  :weight [66.6]
  :height [1.2])

(deffamily (familychain :voltorb :electrode))



(deflearnset :rb
  :voltorb [:bide :double-team :explosion :flash :light-screen :mimic :rage :reflect :rest :screech :selfdestruct :sonicboom :substitute :swift :tackle :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic]
  :electrode [:hyper-beam :skull-bash])

(deflearnset :gs
  :voltorb [:curse :double-team :endure :explosion :flash :frustration :headbutt :hidden-power :light-screen :mirror-coat :protect :rain-dance :rest :return :rollout :screech :selfdestruct :sleep-talk :snore :sonicboom :swagger :swift :tackle :thunder :thunderbolt :toxic :zap-cannon]
  :electrode [:hyper-beam])

(deflearnset :rs
  :voltorb [:charge :double-team :endure :explosion :facade :flash :frustration :hidden-power :light-screen :mimic :mirror-coat :protect :rain-dance :rest :return :rollout :screech :secret-power :selfdestruct :shock-wave :sleep-talk :snore :sonicboom :spark :substitute :swagger :swift :tackle :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :mimic :refresh]
  :electrode [:hyper-beam :mimic])

(deflearnset :dp
  :voltorb [:charge :charge-beam :double-team :endure :explosion :facade :flash :frustration :gyro-ball :headbutt :hidden-power :light-screen :magic-coat :magnet-rise :mirror-coat :natural-gift :protect :rain-dance :rest :return :rollout :screech :secret-power :selfdestruct :shock-wave :signal-beam :sleep-talk :snore :sonicboom :spark :substitute :sucker-punch :swagger :swift :tackle :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :mimic :refresh]
  :electrode [:giga-impact :hyper-beam :mimic])

(deflearnset :bw
  :voltorb [:charge :charge-beam :double-team :electro-ball :explosion :facade :flash :foul-play :frustration :gyro-ball :hidden-power :light-screen :magic-coat :magnet-rise :mirror-coat :protect :rain-dance :rest :return :rollout :round :screech :selfdestruct :signal-beam :sleep-talk :snore :sonicboom :spark :substitute :swagger :swift :tackle :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :volt-switch :wild-charge :mimic :refresh]
  :electrode [:giga-impact :hyper-beam :mimic])

