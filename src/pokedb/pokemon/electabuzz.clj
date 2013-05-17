
(in-ns 'smogon.dex)


(defpokemon :elekid
  :name "Elekid"
  :introduced-in :gs
  :types [[:electric]]
  :egggroups [[]]
  :abilities [[:static]
              :bw [:static :vital-spirit]]
  :stats [[45 63 37 65 55 95]]
  :weight [23.5]
  :height [0.6])

(defpokemon :electabuzz
  :name "Electabuzz"
  :introduced-in :rb
  :types [[:electric]]
  :egggroups [[:humanshape]]
  :abilities [[:static]
              :bw [:static :vital-spirit]]
  :stats [[65 83 57 85 105]
          :gs [65 83 57 95 85 105]]
  :weight [30]
  :height [1.1])

(defpokemon :electivire
  :name "Electivire"
  :introduced-in :dp
  :types [[:electric]]
  :egggroups [[:humanshape]]
  :abilities [[:motor-drive :vital-spirit]]
  :stats [[75 123 67 95 85 95]]
  :weight [138.6]
  :height [1.8])

(deffamily (familychain :elekid :electabuzz :electivire))

(deflearnset :rb
  :electabuzz [:bide :body-slam :counter :double-edge :double-team :flash :hyper-beam :leer :light-screen :mega-kick :mega-punch :metronome :mimic :psychic :psywave :quick-attack :rage :reflect :rest :screech :seismic-toss :skull-bash :strength :submission :substitute :swift :take-down :teleport :thunder :thunder-wave :thunderbolt :thunderpunch :thundershock :toxic])

(deflearnset :gs
  :elekid [:attract :barrier :cross-chop :curse :detect :double-team :dynamicpunch :endure :fire-punch :flash :frustration :headbutt :hidden-power :ice-punch :karate-chop :leer :light-screen :meditate :mud-slap :protect :psychic :quick-attack :rain-dance :rest :return :rolling-kick :screech :sleep-talk :snore :swagger :swift :thief :thunder :thunderbolt :thunderpunch :toxic :zap-cannon]
  :electabuzz [:hyper-beam :iron-tail :rock-smash :strength])

(deflearnset :rs
  :elekid [:attract :barrier :body-slam :brick-break :counter :cross-chop :double-edge :double-team :dynamicpunch :endure :facade :fire-punch :flash :focus-punch :frustration :hidden-power :ice-punch :karate-chop :leer :light-screen :meditate :mega-kick :mega-punch :mimic :mud-slap :protect :psychic :quick-attack :rain-dance :rest :return :rock-smash :rolling-kick :screech :secret-power :seismic-toss :shock-wave :sleep-talk :snore :substitute :swagger :swift :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :body-slam :double-edge :mimic :seismic-toss]
  :electabuzz [:hyper-beam :iron-tail :strength :body-slam :double-edge :follow-me :mimic :seismic-toss])

(deflearnset :dp
  :elekid [:attract :barrier :brick-break :captivate :charge-beam :cross-chop :discharge :double-team :dynamicpunch :endure :facade :feint :fire-punch :flash :fling :focus-punch :frustration :headbutt :helping-hand :hidden-power :ice-punch :karate-chop :leer :light-screen :low-kick :magnet-rise :meditate :mud-slap :natural-gift :protect :psychic :quick-attack :rain-dance :rest :return :rock-smash :rolling-kick :screech :secret-power :shock-wave :signal-beam :sleep-talk :snore :substitute :swagger :swift :thief :thunder :thunder-wave :thunderbolt :thunderpunch :thundershock :toxic :uproar :body-slam :double-edge :mimic :seismic-toss]
  :electabuzz [:focus-blast :giga-impact :hyper-beam :iron-tail :rock-climb :strength :body-slam :double-edge :follow-me :mimic :seismic-toss]
  :electivire [:dig :earthquake :flamethrower :rock-slide :rock-tomb :taunt :torment])

(deflearnset :bw
  :elekid [:attract :barrier :brick-break :charge-beam :covet :cross-chop :discharge :double-team :dual-chop :dynamicpunch :electro-ball :electroweb :facade :feint :fire-punch :flash :fling :focus-punch :frustration :hammer-arm :helping-hand :hidden-power :ice-punch :karate-chop :leer :light-screen :low-kick :magnet-rise :meditate :protect :psychic :quick-attack :rain-dance :rest :return :rock-smash :rolling-kick :round :screech :shock-wave :signal-beam :sleep-talk :snore :substitute :swagger :swift :thief :thunder :thunder-wave :thunderbolt :thunderpunch :thundershock :toxic :uproar :volt-switch :wild-charge :body-slam :double-edge :mimic :seismic-toss]
  :electabuzz [:focus-blast :giga-impact :hyper-beam :iron-tail :low-sweep :strength :body-slam :double-edge :follow-me :mimic :seismic-toss]
  :electivire [:bulldoze :dig :earthquake :flamethrower :rock-slide :rock-tomb :taunt :torment])
