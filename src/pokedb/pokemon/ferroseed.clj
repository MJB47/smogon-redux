
(in-ns 'smogon.dex)


(defpokemon :ferroseed
  :name "Ferroseed"
  :introduced-in :bw
  :types [[:grass :steel]]
  :egggroups [[:mineral :plant]]
  :abilities [[:iron-barbs]]
  :stats [[44 50 91 24 86 10]]
  :weight [18.8]
  :height [0.6])

(defpokemon :ferrothorn
  :name "Ferrothorn"
  :introduced-in :bw
  :types [[:grass :steel]]
  :egggroups [[:mineral :plant]]
  :abilities [[:iron-barbs]]
  :stats [[74 94 131 54 116 20]]
  :weight [110]
  :height [1])

(deffamily (familychain :ferroseed :ferrothorn))

(deflearnset :bw
  :ferroseed [:bullet-seed :curse :double-team :endeavor :energy-ball :explosion :facade :flash :flash-cannon :frustration :giga-drain :gravity :gyro-ball :harden :hidden-power :hone-claws :ingrain :iron-defense :iron-head :leech-seed :magnet-rise :metal-claw :mirror-shot :payback :pin-missile :poison-jab :protect :rest :return :rock-climb :rock-polish :rock-smash :rollout :round :seed-bomb :selfdestruct :sleep-talk :snore :solarbeam :spikes :stealth-rock :substitute :sunny-day :swagger :tackle :thunder-wave :thunderbolt :toxic :worry-seed]
  :ferrothorn [:aerial-ace :bulldoze :cut :giga-impact :grass-knot :hyper-beam :knock-off :power-whip :sandstorm :shadow-claw :strength :swords-dance :thunder])
