
(in-ns 'smogon.dex)


(defpokemon :petilil
  :name "Petilil"
  :introduced-in :bw
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll :leaf-guard :own-tempo]]
  :stats [[45 35 50 70 50 30]]
  :weight [6.6]
  :height [0.5])

(defpokemon :lilligant
  :name "Lilligant"
  :introduced-in :bw
  :types [[:grass]]
  :egggroups [[:plant]]
  :abilities [[:chlorophyll :leaf-guard :own-tempo]]
  :stats [[70 60 75 110 75 90]]
  :weight [16.3]
  :height [1.1])

(deffamily (familychain :petilil :lilligant))

(deflearnset :bw
  :petilil [:absorb :after-you :aromatherapy :attract :bide :charm :covet :cut :double-team :dream-eater :endure :energy-ball :entrainment :facade :flash :frustration :giga-drain :grass-knot :grasswhistle :growth :heal-bell :healing-wish :helping-hand :hidden-power :ingrain :leaf-storm :leech-seed :magical-leaf :mega-drain :natural-gift :protect :rest :return :round :safeguard :seed-bomb :sleep-powder :sleep-talk :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :sweet-scent :synthesis :toxic :worry-seed]
  :lilligant [:giga-impact :hyper-beam :light-screen :petal-dance :quiver-dance :role-play :swords-dance :teeter-dance])
