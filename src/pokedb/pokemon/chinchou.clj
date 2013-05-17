
(in-ns 'smogon.dex)


(defpokemon :chinchou
  :name "Chinchou"
  :introduced-in :gs
  :types [[:electric :water]]
  :egggroups [[:water-2]]
  :abilities [[:illuminate :volt-absorb]
              :bw [:illuminate :volt-absorb :water-absorb]]
  :stats [[75 38 38 56 56 67]]
  :weight [12]
  :height [0.5])

(defpokemon :lanturn
  :name "Lanturn"
  :introduced-in :gs
  :types [[:electric :water]]
  :egggroups [[:water-2]]
  :abilities [[:illuminate :volt-absorb]
              :bw [:illuminate :volt-absorb :water-absorb]]
  :stats [[125 58 58 76 76 67]]
  :weight [22.5]
  :height [1.2])

(deffamily (familychain :chinchou :lanturn))

(deflearnset :gs
  :chinchou [:attract :bubble :confuse-ray :curse :double-team :endure :flail :flash :frustration :hidden-power :hydro-pump :ice-beam :protect :rain-dance :rest :return :screech :sleep-talk :snore :spark :supersonic :surf :swagger :take-down :thunder :thunder-wave :thunderbolt :toxic :water-gun :waterfall :whirlpool :zap-cannon]
  :lanturn [:hyper-beam])

(deflearnset :rs
  :chinchou [:amnesia :attract :blizzard :bubble :charge :confuse-ray :dive :double-edge :double-team :endure :facade :flail :flash :frustration :hail :hidden-power :hydro-pump :ice-beam :mimic :protect :rain-dance :rest :return :screech :secret-power :shock-wave :sleep-talk :snore :spark :substitute :supersonic :surf :swagger :take-down :thunder :thunder-wave :thunderbolt :toxic :water-gun :water-pulse :waterfall]
  :lanturn [:hyper-beam])

(deflearnset :dp
  :chinchou [:agility :amnesia :aqua-ring :attract :blizzard :bounce :brine :bubble :bubblebeam :captivate :charge :charge-beam :confuse-ray :discharge :dive :double-team :endure :facade :flail :flash :frustration :hail :heal-bell :hidden-power :hydro-pump :ice-beam :icy-wind :mist :natural-gift :protect :psybeam :rain-dance :rest :return :screech :secret-power :shock-wave :signal-beam :sleep-talk :snore :spark :substitute :sucker-punch :supersonic :surf :swagger :take-down :thunder :thunder-wave :thunderbolt :toxic :water-gun :water-pulse :waterfall :whirlpool]
  :lanturn [:aqua-tail :giga-impact :hyper-beam :spit-up :stockpile :swallow])

(deflearnset :bw
  :chinchou [:agility :amnesia :aqua-ring :attract :blizzard :bounce :brine :bubble :bubblebeam :charge :charge-beam :confuse-ray :discharge :dive :double-team :electro-ball :facade :flail :flash :frustration :hail :heal-bell :hidden-power :hydro-pump :ice-beam :icy-wind :mist :protect :psybeam :rain-dance :rest :return :round :scald :screech :shock-wave :signal-beam :sleep-talk :snore :spark :substitute :supersonic :surf :swagger :take-down :thunder :thunder-wave :thunderbolt :toxic :volt-switch :water-gun :water-pulse :waterfall :whirlpool :wild-charge]
  :lanturn [:aqua-tail :giga-impact :hyper-beam :spit-up :stockpile :swallow])
