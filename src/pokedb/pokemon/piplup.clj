
(in-ns 'smogon.dex)

(defpokemon :piplup
  :name "Piplup"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:defiant :torrent]]
  :stats [[53 51 53 61 56 40]]
  :weight [5.2]
  :height [0.4])

(defpokemon :prinplup
  :name "Prinplup"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:defiant :torrent]]
  :stats [[64 66 68 81 76 50]]
  :weight [23]
  :height [0.8])

(defpokemon :empoleon
  :name "Empoleon"
  :introduced-in :dp
  :types [[:steel :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:defiant :torrent]]
  :stats [[84 86 88 111 101 60]]
  :weight [84.5]
  :height [1.7])

(deffamily (familychain :piplup :prinplup :empoleon))

(deflearnset :dp
  :piplup [:aerial-ace :agility :aqua-ring :attract :bide :blizzard :brick-break :brine :bubble :bubblebeam :captivate :cut :defog :dig :dive :double-hit :double-team :drill-peck :endure :facade :featherdance :flail :fling :frustration :fury-attack :grass-knot :growl :hail :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :mist :mud-slap :mud-sport :natural-gift :peck :pluck :pound :protect :rain-dance :rest :return :rock-tomb :secret-power :signal-beam :sleep-talk :snore :stealth-rock :substitute :supersonic :surf :swagger :toxic :water-pulse :water-sport :waterfall :whirlpool :yawn]
  :prinplup [:metal-claw :rock-smash :shadow-claw :strength :tackle]
  :empoleon [:aqua-jet :avalanche :earthquake :flash-cannon :fury-cutter :giga-impact :hydro-cannon :hyper-beam :iron-defense :knock-off :roar :rock-climb :rock-slide :steel-wing :swords-dance])

(deflearnset :bw
  :piplup [:aerial-ace :agility :aqua-ring :attract :bide :blizzard :brick-break :brine :bubble :bubblebeam :covet :cut :dig :dive :double-hit :double-team :drill-peck :echoed-voice :facade :featherdance :flail :fling :frustration :fury-attack :grass-knot :growl :hail :hidden-power :hydro-pump :ice-beam :icy-wind :mist :mud-slap :mud-sport :peck :pluck :pound :protect :quash :rain-dance :rest :return :rock-tomb :round :scald :signal-beam :sleep-talk :snore :stealth-rock :substitute :supersonic :surf :swagger :toxic :water-pledge :water-sport :waterfall :whirlpool :yawn :sing]
  :prinplup [:hone-claws :metal-claw :rock-smash :shadow-claw :strength :tackle]
  :empoleon [:aqua-jet :bulldoze :earthquake :flash-cannon :giga-impact :hydro-cannon :hyper-beam :iron-defense :knock-off :roar :rock-slide :swords-dance])
