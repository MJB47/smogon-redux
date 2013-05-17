
(in-ns 'smogon.dex)


(defpokemon :blitzle
  :name "Blitzle"
  :introduced-in :bw
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:lightningrod :motor-drive :sap-sipper]]
  :stats [[45 60 32 50 32 76]]
  :weight [29.8]
  :height [0.8])

(defpokemon :zebstrika
  :name "Zebstrika"
  :introduced-in :bw
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:lightningrod :motor-drive :sap-sipper]]
  :stats [[75 100 63 80 63 116]]
  :weight [79.5]
  :height [1.6])

(deffamily (familychain :blitzle :zebstrika))

(deflearnset :bw
  :blitzle [:agility :attract :bounce :charge :charge-beam :discharge :double-edge :double-kick :double-team :endure :facade :flame-charge :flash :frustration :hidden-power :light-screen :magnet-rise :me-first :protect :pursuit :quick-attack :rage :rain-dance :rest :return :round :sand-attack :screech :shock-wave :signal-beam :sleep-talk :snore :spark :stomp :substitute :swagger :tail-whip :take-down :thrash :thunder :thunder-wave :thunderbolt :toxic :volt-switch :wild-charge]
  :zebstrika [:giga-impact :hyper-beam :overheat :rock-smash])
