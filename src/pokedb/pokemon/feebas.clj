
(in-ns 'smogon.dex)


(defpokemon :feebas
  :name "Feebas"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:swift-swim]
              :bw [:adaptability :swift-swim]]
  :stats [[20 15 20 10 55 80]]
  :weight [7.4]
  :height [0.6])

(defpokemon :milotic
  :name "Milotic"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:marvel-scale]
              :bw [:cute-charm :marvel-scale]]
  :stats [[95 60 79 100 125 81]]
  :weight [162]
  :height [6.2])

(deffamily (familychain :feebas :milotic))

(deflearnset :rs
  :feebas [:attract :blizzard :confuse-ray :dive :double-edge :double-team :dragonbreath :endure :facade :flail :frustration :hail :hidden-power :hypnosis :ice-beam :icy-wind :light-screen :mimic :mirror-coat :mud-sport :protect :rain-dance :rest :return :secret-power :sleep-talk :snore :splash :substitute :surf :swagger :swift :tackle :toxic :water-pulse :waterfall :double-edge :icy-wind :mimic]
  :milotic [:body-slam :hydro-pump :hyper-beam :iron-tail :mud-slap :psych-up :recover :refresh :safeguard :twister :water-gun :water-sport :wrap :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :feebas [:attract :blizzard :captivate :confuse-ray :dive :double-team :dragonbreath :endure :facade :flail :frustration :hail :haze :hidden-power :hypnosis :ice-beam :icy-wind :light-screen :mirror-coat :mist :mud-sport :natural-gift :protect :rain-dance :rest :return :secret-power :sleep-talk :snore :splash :substitute :surf :swagger :swift :tackle :tickle :toxic :water-pulse :waterfall :whirlpool :double-edge :icy-wind :mimic]
  :milotic [:aqua-ring :aqua-tail :avalanche :dragon-pulse :giga-impact :hydro-pump :hyper-beam :iron-head :iron-tail :magic-coat :mud-slap :psych-up :recover :refresh :safeguard :twister :water-gun :water-sport :wrap :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :feebas [:attract :blizzard :brine :captivate :confuse-ray :dive :double-team :dragon-pulse :dragonbreath :facade :flail :frustration :hail :haze :hidden-power :hypnosis :ice-beam :icy-wind :iron-tail :light-screen :mirror-coat :mist :mud-sport :protect :rain-dance :rest :return :round :scald :sleep-talk :snore :splash :substitute :surf :swagger :tackle :tickle :toxic :waterfall :double-edge :icy-wind :mimic]
  :milotic [:aqua-ring :aqua-tail :bind :bulldoze :dragon-tail :giga-impact :hydro-pump :hyper-beam :iron-head :magic-coat :psych-up :recover :refresh :safeguard :twister :water-gun :water-pulse :water-sport :wrap :body-slam :double-edge :icy-wind :mimic])
