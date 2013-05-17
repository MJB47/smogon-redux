
(in-ns 'smogon.dex)

(defpokemon :manaphy
  :name "Manaphy"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:fairy :water-1]]
  :abilities [[:hydration]]
  :stats [[100 100 100 100 100 100]]
  :weight [1.4]
  :height [0.3])

(deffamily (familychain :manaphy))

(deflearnset :dp
  :manaphy [:acid-armor :ancientpower :aqua-ring :blizzard :bounce :brine :bubble :bubblebeam :calm-mind :charm :dive :double-team :endure :energy-ball :facade :flash :fling :frustration :giga-impact :grass-knot :hail :heal-bell :heart-swap :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :knock-off :last-resort :light-screen :mud-slap :natural-gift :protect :psych-up :psychic :rain-dance :reflect :rest :return :safeguard :secret-power :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :substitute :supersonic :surf :swagger :swift :tail-glow :toxic :u-turn :uproar :water-pulse :water-sport :waterfall :whirlpool])

(deflearnset :bw
  :manaphy [:acid-armor :aqua-ring :blizzard :bounce :bubble :bubblebeam :calm-mind :charm :covet :dive :double-team :energy-ball :facade :flash :fling :frustration :giga-impact :grass-knot :hail :heal-bell :heart-swap :helping-hand :hidden-power :hyper-beam :ice-beam :icy-wind :knock-off :last-resort :light-screen :protect :psych-up :psychic :rain-dance :reflect :rest :return :round :safeguard :scald :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :substitute :supersonic :surf :swagger :tail-glow :toxic :u-turn :uproar :water-pulse :water-sport :waterfall :whirlpool])
