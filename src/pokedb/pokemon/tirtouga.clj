
(in-ns 'smogon.dex)


(defpokemon :tirtouga
  :name "Tirtouga"
  :introduced-in :bw
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:solid-rock :sturdy :swift-swim]]
  :stats [[54 78 103 53 45 22]]
  :weight [16.5]
  :height [0.7])

(defpokemon :carracosta
  :name "Carracosta"
  :introduced-in :bw
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:solid-rock :sturdy :swift-swim]]
  :stats [[74 108 133 83 65 32]]
  :weight [81]
  :height [1.2])

(deffamily (familychain :tirtouga :carracosta))



(deflearnset :bw
  :tirtouga [:ancientpower :aqua-jet :aqua-tail :attract :bide :bite :blizzard :block :body-slam :brine :bulldoze :crunch :curse :dig :dive :double-team :earth-power :earthquake :facade :flail :frustration :hidden-power :hydro-pump :ice-beam :icy-wind :iron-defense :iron-tail :knock-off :protect :rain-dance :rest :return :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :round :sandstorm :scald :shell-smash :slam :sleep-talk :smack-down :snore :stealth-rock :stone-edge :strength :substitute :surf :swagger :toxic :water-gun :water-pulse :waterfall :whirlpool :wide-guard :withdraw]
  :carracosta [:focus-blast :giga-impact :hyper-beam :iron-head :low-kick :superpower])

