
(in-ns 'smogon.dex)


(defpokemon :pineco
  :name "Pineco"
  :introduced-in :gs
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:sturdy]
              :bw [:overcoat :sturdy]]
  :stats [[50 65 90 35 35 15]]
  :weight [7.2]
  :height [0.6])

(defpokemon :forretress
  :name "Forretress"
  :introduced-in :gs
  :types [[:bug :steel]]
  :egggroups [[:bug]]
  :abilities [[:sturdy]
              :bw [:overcoat :sturdy]]
  :stats [[75 90 140 60 60 40]]
  :weight [125.8]
  :height [1.2])

(deffamily (familychain :pineco :forretress))

(deflearnset :gs
  :pineco [:attract :bide :curse :defense-curl :double-edge :double-team :endure :explosion :flail :frustration :giga-drain :headbutt :hidden-power :pin-missile :protect :rapid-spin :reflect :rest :return :rock-smash :rollout :selfdestruct :sleep-talk :snore :solarbeam :spikes :strength :sunny-day :swagger :sweet-scent :swift :tackle :take-down :toxic]
  :forretress [:hyper-beam :sandstorm])

(deflearnset :rs
  :pineco [:attract :bide :body-slam :counter :defense-curl :dig :double-edge :double-team :earthquake :endure :explosion :facade :flail :frustration :giga-drain :hidden-power :light-screen :mimic :pin-missile :protect :rapid-spin :reflect :rest :return :rock-slide :rock-smash :rollout :sand-tomb :sandstorm :secret-power :selfdestruct :sleep-talk :snore :solarbeam :spikes :strength :substitute :sunny-day :swagger :swift :tackle :take-down :toxic :body-slam :mimic :refresh]
  :forretress [:hyper-beam :zap-cannon :body-slam :mimic])

(deflearnset :dp
  :pineco [:attract :bide :bug-bite :captivate :counter :dig :double-edge :double-team :earthquake :endure :explosion :facade :flail :frustration :giga-drain :gravity :gyro-ball :headbutt :hidden-power :iron-defense :light-screen :natural-gift :pain-split :payback :pin-missile :power-trick :protect :rapid-spin :reflect :rest :return :revenge :rock-slide :rock-smash :rock-tomb :rollout :sand-tomb :sandstorm :secret-power :selfdestruct :sleep-talk :snore :solarbeam :spikes :stealth-rock :strength :string-shot :substitute :sunny-day :swagger :swift :tackle :take-down :toxic :toxic-spikes :body-slam :mimic :refresh]
  :forretress [:block :flash-cannon :giga-impact :hyper-beam :magnet-rise :mirror-shot :rock-polish :signal-beam :zap-cannon :body-slam :mimic])

(deflearnset :bw
  :pineco [:attract :bide :bug-bite :bulldoze :counter :dig :double-edge :double-team :drill-run :earthquake :endure :explosion :facade :flail :frustration :giga-drain :gravity :gyro-ball :hidden-power :iron-defense :light-screen :natural-gift :pain-split :payback :pin-missile :power-trick :protect :rapid-spin :reflect :rest :return :revenge :rock-slide :rock-smash :rock-tomb :round :sand-tomb :sandstorm :selfdestruct :sleep-talk :snore :solarbeam :spikes :stealth-rock :strength :struggle-bug :substitute :sunny-day :swagger :swift :tackle :take-down :toxic :toxic-spikes :venoshock :body-slam :mimic :refresh]
  :forretress [:autotomize :block :flash-cannon :giga-impact :heavy-slam :hyper-beam :magnet-rise :mirror-shot :rock-polish :signal-beam :volt-switch :zap-cannon :body-slam :mimic])
