
(in-ns 'smogon.dex)


(defpokemon :goldeen
  :name "Goldeen"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-2]]
  :abilities [[:swift-swim :water-veil]
              :bw [:lightningrod :swift-swim :water-veil]]
  :stats [[45 67 60 50 63]
          :gs [45 67 60 35 50 63]]
  :weight [15]
  :height [0.6])

(defpokemon :seaking
  :name "Seaking"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-2]]
  :abilities [[:swift-swim :water-veil]
              :bw [:lightningrod :swift-swim :water-veil]]
  :stats [[80 92 65 80 68]
          :gs [80 92 65 65 80 68]]
  :weight [39]
  :height [1.3])

(deffamily (familychain :goldeen :seaking))

(deflearnset :rb
  :goldeen [:agility :bide :blizzard :bubblebeam :double-edge :double-team :fury-attack :horn-attack :horn-drill :ice-beam :mimic :peck :rage :rest :skull-bash :substitute :supersonic :surf :swift :tail-whip :take-down :toxic :water-gun :waterfall]
  :seaking [:hyper-beam])

(deflearnset :gs
  :goldeen [:agility :attract :blizzard :curse :double-team :endure :flail :frustration :fury-attack :haze :hidden-power :horn-attack :horn-drill :hydro-pump :ice-beam :icy-wind :peck :protect :psybeam :rain-dance :rest :return :sleep-talk :snore :supersonic :surf :swagger :swift :tail-whip :toxic :waterfall]
  :seaking [:hyper-beam])

(deflearnset :rs
  :goldeen [:agility :attract :blizzard :dive :double-edge :double-team :endure :facade :flail :frustration :fury-attack :hail :haze :hidden-power :horn-attack :horn-drill :hydro-pump :ice-beam :icy-wind :megahorn :mimic :mud-sport :peck :protect :psybeam :rain-dance :rest :return :secret-power :sleep-talk :snore :substitute :supersonic :surf :swagger :swift :tail-whip :toxic :water-pulse :water-sport :waterfall :double-edge :icy-wind :mimic]
  :seaking [:hyper-beam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :goldeen [:agility :aqua-ring :aqua-tail :attract :blizzard :body-slam :bounce :captivate :dive :double-team :endure :facade :flail :frustration :fury-attack :fury-cutter :hail :haze :hidden-power :horn-attack :horn-drill :hydro-pump :ice-beam :icy-wind :knock-off :megahorn :mud-slap :mud-sport :natural-gift :peck :poison-jab :protect :psybeam :rain-dance :rest :return :secret-power :sleep-talk :snore :substitute :supersonic :surf :swagger :swift :tail-whip :toxic :water-pulse :water-sport :waterfall :whirlpool :double-edge :icy-wind :mimic]
  :seaking [:giga-impact :hyper-beam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :goldeen [:agility :aqua-ring :aqua-tail :attract :blizzard :body-slam :bounce :dive :double-team :drill-run :facade :flail :frustration :fury-attack :hail :haze :hidden-power :horn-attack :horn-drill :hydro-pump :ice-beam :icy-wind :knock-off :megahorn :mud-shot :mud-slap :mud-sport :peck :poison-jab :protect :psybeam :rain-dance :rest :return :round :scald :signal-beam :skull-bash :sleep-talk :snore :soak :substitute :supersonic :surf :swagger :tail-whip :toxic :water-pulse :water-sport :waterfall :double-edge :icy-wind :mimic]
  :seaking [:giga-impact :hyper-beam :double-edge :icy-wind :mimic])
