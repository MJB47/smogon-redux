
(in-ns 'smogon.dex)


(defpokemon :mantyke
  :name "Mantyke"
  :introduced-in :dp
  :types [[:flying :water]]
  :egggroups [[]]
  :abilities [[:swift-swim :water-absorb :water-veil]]
  :stats [[45 20 50 60 120 50]]
  :weight [65]
  :height [1])

(defpokemon :mantine
  :name "Mantine"
  :introduced-in :gs
  :types [[:flying :water]]
  :egggroups [[:water-1]]
  :abilities [[:swift-swim :water-absorb]
              :bw [:swift-swim :water-absorb :water-veil]]
  :stats [[65 40 70 80 140 70]]
  :weight [220]
  :height [2.1])

(deffamily (familychain :mantyke :mantine))

(deflearnset :gs
  :mantine [:agility :attract :blizzard :bubble :bubblebeam :confuse-ray :curse :double-team :endure :frustration :haze :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :mud-slap :protect :rain-dance :rest :return :slam :sleep-talk :snore :supersonic :surf :swagger :swift :tackle :take-down :toxic :twister :waterfall :whirlpool :wing-attack])

(deflearnset :rs
  :mantine [:aerial-ace :agility :attract :blizzard :body-slam :bubble :bubblebeam :confuse-ray :dive :double-edge :double-team :earthquake :endure :facade :frustration :hail :haze :hidden-power :hydro-pump :ice-beam :icy-wind :mimic :mud-slap :mud-sport :protect :rain-dance :rest :return :rock-slide :secret-power :slam :sleep-talk :snore :substitute :supersonic :surf :swagger :swift :tackle :take-down :toxic :twister :water-pulse :waterfall :wing-attack :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :mantyke [:aerial-ace :agility :air-cutter :aqua-ring :attract :blizzard :bounce :bubble :bubblebeam :captivate :confuse-ray :dive :double-team :earthquake :endure :facade :frustration :hail :haze :headbutt :helping-hand :hidden-power :hydro-pump :ice-beam :icy-wind :mirror-coat :mud-slap :mud-sport :natural-gift :protect :rain-dance :rest :return :rock-slide :secret-power :signal-beam :slam :sleep-talk :snore :splash :substitute :supersonic :surf :swagger :swift :tackle :take-down :toxic :twister :water-pulse :water-sport :waterfall :whirlpool :wing-attack]
  :mantine [:aqua-tail :brine :bullet-seed :defog :giga-impact :gunk-shot :hyper-beam :iron-head :psybeam :rock-tomb :seed-bomb :string-shot :tailwind :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :mantyke [:acrobatics :aerial-ace :agility :air-slash :amnesia :aqua-ring :attract :blizzard :bounce :bubble :bubblebeam :bulldoze :confuse-ray :dive :double-team :earthquake :facade :frustration :hail :haze :headbutt :helping-hand :hidden-power :hydro-pump :ice-beam :icy-wind :mirror-coat :mud-sport :protect :rain-dance :rest :return :rock-slide :round :scald :signal-beam :slam :sleep-talk :snore :splash :substitute :supersonic :surf :swagger :tackle :take-down :toxic :twister :water-pulse :water-sport :waterfall :wide-guard :wing-attack]
  :mantine [:aqua-tail :bullet-seed :giga-impact :gunk-shot :hyper-beam :iron-head :psybeam :rock-tomb :seed-bomb :tailwind :body-slam :double-edge :icy-wind :mimic])
