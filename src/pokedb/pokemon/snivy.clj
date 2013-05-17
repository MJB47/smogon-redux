
(in-ns 'smogon.dex)


(defpokemon :snivy
  :name "Snivy"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:ground :plant]]
  :abilities [[:contrary :overgrow]]
  :stats [[45 45 55 45 55 63]]
  :weight [8.1]
  :height [0.6])

(defpokemon :servine
  :name "Servine"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:ground :plant]]
  :abilities [[:contrary :overgrow]]
  :stats [[60 60 75 60 75 83]]
  :weight [16]
  :height [0.8])

(defpokemon :serperior
  :name "Serperior"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:ground :plant]]
  :abilities [[:contrary :overgrow]]
  :stats [[75 75 95 75 95 113]]
  :weight [63]
  :height [3.3])

(deffamily (familychain :snivy :servine :serperior))

(deflearnset :bw
  :snivy [:aerial-ace :aqua-tail :attract :bind :calm-mind :captivate :coil :cut :double-team :energy-ball :facade :flash :frustration :gastro-acid :giga-drain :glare :grass-knot :grass-pledge :growth :hidden-power :iron-tail :knock-off :leaf-blade :leaf-storm :leaf-tornado :leech-seed :leer :light-screen :magical-leaf :mean-look :mega-drain :mirror-coat :natural-gift :protect :pursuit :reflect :rest :return :round :safeguard :seed-bomb :slam :sleep-talk :snatch :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :tackle :taunt :torment :toxic :twister :vine-whip :worry-seed :wrap :wring-out]
  :serperior [:dragon-pulse :dragon-tail :frenzy-plant :giga-impact :hyper-beam :outrage :rock-smash :strength])
