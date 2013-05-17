
(in-ns 'smogon.dex)


(defpokemon :remoraid
  :name "Remoraid"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:water-1 :water-2]]
  :abilities [[:hustle]
              :dp [:hustle :sniper]
              :bw [:hustle :moody :sniper]]
  :stats [[35 65 35 65 35 65]]
  :weight [12]
  :height [0.6])

(defpokemon :octillery
  :name "Octillery"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:water-1 :water-2]]
  :abilities [[:suction-cups]
              :dp [:sniper :suction-cups]
              :bw [:moody :sniper :suction-cups]]
  :stats [[75 105 75 105 75 45]]
  :weight [28.5]
  :height [0.9])

(deffamily (familychain :remoraid :octillery))

(deflearnset :gs
  :remoraid [:attract :aurora-beam :bubblebeam :curse :defense-curl :double-team :endure :flamethrower :focus-energy :frustration :haze :hidden-power :hyper-beam :ice-beam :lock-on :mud-slap :octazooka :protect :psybeam :rain-dance :rest :return :screech :sleep-talk :snore :supersonic :surf :swagger :swift :thief :toxic :water-gun :whirlpool]
  :octillery [:constrict])

(deflearnset :rs
  :remoraid [:attract :aurora-beam :blizzard :bubblebeam :defense-curl :dive :double-edge :double-team :endure :facade :fire-blast :flamethrower :focus-energy :frustration :haze :hidden-power :hyper-beam :ice-beam :lock-on :mimic :mud-slap :octazooka :protect :psybeam :psychic :rain-dance :rest :return :rock-blast :screech :secret-power :sleep-talk :snore :substitute :sunny-day :supersonic :surf :swagger :swift :thief :thunder-wave :toxic :water-gun :water-pulse :waterfall :double-edge :mimic]
  :octillery [:bullet-seed :constrict :seismic-toss :sludge-bomb :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :remoraid [:attract :aurora-beam :blizzard :bounce :brine :bubblebeam :bullet-seed :captivate :charge-beam :dive :double-team :endure :facade :fire-blast :flail :flamethrower :focus-energy :frustration :gunk-shot :haze :hidden-power :hyper-beam :ice-beam :icy-wind :lock-on :mud-slap :natural-gift :octazooka :protect :psybeam :psychic :rain-dance :rest :return :rock-blast :screech :secret-power :seed-bomb :signal-beam :sleep-talk :snore :string-shot :substitute :sunny-day :supersonic :surf :swagger :swift :thief :thunder-wave :toxic :water-gun :water-pulse :water-spout :waterfall :whirlpool :double-edge :mimic]
  :octillery [:constrict :energy-ball :flash-cannon :giga-impact :payback :sludge-bomb :wring-out :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :remoraid [:acid-spray :attract :aurora-beam :blizzard :bounce :bubblebeam :bullet-seed :charge-beam :dive :double-team :facade :fire-blast :flail :flamethrower :focus-energy :frustration :gunk-shot :haze :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :incinerate :lock-on :mud-shot :octazooka :protect :psybeam :psychic :rain-dance :rest :return :rock-blast :round :scald :screech :seed-bomb :signal-beam :sleep-talk :smack-down :snore :soak :substitute :sunny-day :supersonic :surf :swagger :swift :thief :thunder-wave :toxic :water-gun :water-pulse :water-spout :waterfall :double-edge :mimic]
  :octillery [:bind :constrict :energy-ball :flash-cannon :giga-impact :payback :sludge-bomb :sludge-wave :wring-out :double-edge :mimic :seismic-toss])
