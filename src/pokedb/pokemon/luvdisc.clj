
(in-ns 'smogon.dex)


(defpokemon :luvdisc
  :name "Luvdisc"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:water-2]]
  :abilities [[:swift-swim]
              :bw [:hydration :swift-swim]]
  :stats [[43 30 55 40 65 97]]
  :weight [8.7]
  :height [0.6])

(deffamily (familychain :luvdisc))

(deflearnset :rs
  :luvdisc [:agility :attract :blizzard :charm :dive :double-edge :double-team :endure :facade :flail :frustration :hail :hidden-power :ice-beam :icy-wind :mimic :mud-sport :protect :psych-up :rain-dance :rest :return :safeguard :secret-power :sleep-talk :snore :splash :substitute :supersonic :surf :swagger :sweet-kiss :swift :tackle :take-down :toxic :water-gun :water-pulse :water-sport :waterfall :double-edge :icy-wind :mimic])

(deflearnset :dp
  :luvdisc [:agility :aqua-jet :aqua-ring :attract :blizzard :bounce :brine :captivate :charm :dive :double-team :endure :facade :flail :frustration :hail :hidden-power :ice-beam :icy-wind :lucky-chant :mud-sport :natural-gift :protect :psych-up :rain-dance :rest :return :safeguard :secret-power :sleep-talk :snore :splash :substitute :supersonic :surf :swagger :sweet-kiss :swift :tackle :take-down :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool :double-edge :icy-wind :mimic])

(deflearnset :bw
  :luvdisc [:agility :aqua-jet :aqua-ring :attract :blizzard :bounce :brine :captivate :charm :dive :double-team :facade :flail :frustration :hail :heal-pulse :hidden-power :hydro-pump :ice-beam :icy-wind :lucky-chant :mud-sport :protect :psych-up :rain-dance :rest :return :round :safeguard :scald :sleep-talk :snore :splash :substitute :supersonic :surf :swagger :sweet-kiss :tackle :take-down :toxic :water-gun :water-pulse :water-sport :waterfall :double-edge :icy-wind :mimic])
