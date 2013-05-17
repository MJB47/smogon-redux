
(in-ns 'smogon.dex)


(defpokemon :finneon
  :name "Finneon"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:water-2]]
  :abilities [[:storm-drain :swift-swim :water-veil]]
  :stats [[49 49 56 49 61 66]]
  :weight [7]
  :height [0.4])

(defpokemon :lumineon
  :name "Lumineon"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:water-2]]
  :abilities [[:storm-drain :swift-swim :water-veil]]
  :stats [[69 69 76 69 86 91]]
  :weight [24]
  :height [1.2])

(deffamily (familychain :finneon :lumineon))

(deflearnset :dp
  :finneon [:agility :air-cutter :aqua-ring :aqua-tail :attract :blizzard :bounce :brine :captivate :charm :defog :dive :double-team :endure :facade :flail :flash :frustration :gust :hail :hidden-power :ice-beam :icy-wind :natural-gift :ominous-wind :payback :pound :protect :psybeam :psych-up :rain-dance :rest :return :safeguard :secret-power :silver-wind :sleep-talk :snore :splash :substitute :surf :swagger :sweet-kiss :swift :tailwind :tickle :toxic :twister :u-turn :water-gun :water-pulse :waterfall :whirlpool]
  :lumineon [:giga-impact :hyper-beam])

(deflearnset :bw
  :finneon [:agility :aqua-ring :aqua-tail :attract :aurora-beam :blizzard :bounce :brine :captivate :charm :dive :double-team :facade :flail :flash :frustration :gust :hail :hidden-power :ice-beam :icy-wind :payback :pound :protect :psybeam :psych-up :rain-dance :rest :return :round :safeguard :scald :signal-beam :silver-wind :sleep-talk :snore :soak :splash :substitute :surf :swagger :sweet-kiss :tailwind :tickle :toxic :u-turn :water-gun :water-pulse :waterfall :whirlpool]
  :lumineon [:giga-impact :hyper-beam])
