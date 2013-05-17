
(in-ns 'smogon.dex)

(defpokemon :phione
  :name "Phione"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:fairy :water-1]]
  :abilities [[:hydration]]
  :stats [[80 80 80 80 80 80]]
  :weight [3.1]
  :height [0.4])

(deffamily (familychain :phione))

(deflearnset :dp
  :phione [:acid-armor :ancientpower :aqua-ring :blizzard :bounce :brine :bubble :bubblebeam :charm :dive :double-team :endure :facade :fling :frustration :grass-knot :hail :heal-bell :helping-hand :hidden-power :ice-beam :icy-wind :knock-off :last-resort :mud-slap :natural-gift :protect :psych-up :rain-dance :rest :return :safeguard :secret-power :signal-beam :sleep-talk :snore :substitute :supersonic :surf :swagger :swift :toxic :u-turn :uproar :water-pulse :water-sport :waterfall :whirlpool])

(deflearnset :bw
  :phione [:acid-armor :aqua-ring :blizzard :bounce :bubble :bubblebeam :charm :covet :dive :double-team :facade :fling :frustration :grass-knot :hail :heal-bell :helping-hand :hidden-power :ice-beam :icy-wind :knock-off :last-resort :protect :psych-up :rain-dance :rest :return :round :safeguard :scald :signal-beam :sleep-talk :snore :substitute :supersonic :surf :swagger :toxic :u-turn :uproar :water-pulse :water-sport :waterfall :whirlpool])
