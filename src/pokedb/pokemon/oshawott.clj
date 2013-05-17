
(in-ns 'smogon.dex)


(defpokemon :oshawott
  :name "Oshawott"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground]]
  :abilities [[:shell-armor :torrent]]
  :stats [[55 55 45 63 45 45]]
  :weight [5.9]
  :height [0.5])

(defpokemon :dewott
  :name "Dewott"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground]]
  :abilities [[:shell-armor :torrent]]
  :stats [[75 75 60 83 60 60]]
  :weight [24.5]
  :height [0.8])

(defpokemon :samurott
  :name "Samurott"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground]]
  :abilities [[:shell-armor :torrent]]
  :stats [[95 100 85 108 70 70]]
  :weight [94.6]
  :height [1.5])

(deffamily (familychain :oshawott :dewott :samurott))

(deflearnset :bw
  :oshawott [:aerial-ace :air-slash :aqua-jet :aqua-tail :assurance :attract :blizzard :brine :copycat :covet :cut :detect :dig :dive :double-team :encore :facade :false-swipe :fling :focus-energy :frustration :fury-cutter :grass-knot :hail :helping-hand :hidden-power :hydro-pump :ice-beam :icy-wind :iron-tail :night-slash :protect :rain-dance :razor-shell :rest :retaliate :return :revenge :rock-smash :round :scald :screech :sleep-talk :snore :substitute :surf :swagger :swords-dance :tackle :tail-whip :taunt :toxic :trump-card :water-gun :water-pledge :water-pulse :water-sport :waterfall :x-scissor]
  :samurott [:block :dragon-tail :giga-impact :hydro-cannon :hyper-beam :knock-off :megahorn :slash :strength :superpower])
