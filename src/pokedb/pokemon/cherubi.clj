
(in-ns 'smogon.dex)


(defpokemon :cherubi
  :name "Cherubi"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:chlorophyll]]
  :stats [[45 35 45 62 53 35]]
  :weight [3.3]
  :height [0.4])

(defpokemon :cherrim
  :name "Cherrim"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:flower-gift]]
  :stats [[70 60 70 87 78 85]]
  :weight [9.3]
  :height [0.5])

(deffamily (familychain :cherubi :cherrim))

(deflearnset :dp
  :cherubi [:aromatherapy :attract :bullet-seed :captivate :double-team :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :helping-hand :hidden-power :leech-seed :lucky-chant :magical-leaf :natural-gift :nature-power :protect :razor-leaf :rest :return :rollout :safeguard :secret-power :seed-bomb :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :take-down :tickle :toxic :weather-ball :worry-seed]
  :cherrim [:giga-impact :hyper-beam :petal-dance])

(deflearnset :bw
  :cherubi [:aromatherapy :attract :defense-curl :double-team :energy-ball :facade :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :heal-pulse :healing-wish :helping-hand :hidden-power :leech-seed :lucky-chant :magical-leaf :morning-sun :natural-gift :nature-power :protect :razor-leaf :rest :return :rollout :round :safeguard :seed-bomb :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :take-down :tickle :toxic :weather-ball :worry-seed]
  :cherrim [:giga-impact :hyper-beam :petal-dance])
