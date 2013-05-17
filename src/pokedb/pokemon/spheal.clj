
(in-ns 'smogon.dex)


(defpokemon :spheal
  :name "Spheal"
  :introduced-in :rs
  :types [[:ice :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:thick-fat]
              :dp [:ice-body :thick-fat]
              :bw [:ice-body :oblivious :thick-fat]]
  :stats [[70 40 50 55 50 25]]
  :weight [39.5]
  :height [0.8])

(defpokemon :sealeo
  :name "Sealeo"
  :introduced-in :rs
  :types [[:ice :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:thick-fat]
              :dp [:ice-body :thick-fat]
              :bw [:ice-body :oblivious :thick-fat]]
  :stats [[90 60 70 75 70 45]]
  :weight [87.6]
  :height [1.1])

(defpokemon :walrein
  :name "Walrein"
  :introduced-in :rs
  :types [[:ice :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:thick-fat]
              :dp [:ice-body :thick-fat]
              :bw [:ice-body :oblivious :thick-fat]]
  :stats [[110 80 90 95 90 65]]
  :weight [150.6]
  :height [1.4])

(deffamily (familychain :spheal :sealeo :walrein))



(deflearnset :rs
  :spheal [:attract :aurora-beam :blizzard :body-slam :curse :defense-curl :dive :double-edge :double-team :earthquake :encore :endure :facade :fissure :frustration :growl :hail :hidden-power :ice-ball :ice-beam :icy-wind :iron-tail :mimic :mud-slap :powder-snow :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :secret-power :sheer-cold :sleep-talk :snore :spit-up :stockpile :strength :substitute :surf :swagger :swallow :toxic :water-gun :water-pulse :water-sport :waterfall :yawn :charm :double-edge :icy-wind :mimic :mud-slap]
  :sealeo [:roar :double-edge :icy-wind :mimic]
  :walrein [:hyper-beam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :spheal [:aqua-ring :aqua-tail :attract :aurora-beam :blizzard :body-slam :brine :captivate :curse :defense-curl :dive :double-team :earthquake :encore :endure :facade :fissure :frustration :growl :hail :headbutt :hidden-power :ice-ball :ice-beam :icy-wind :iron-tail :mud-slap :natural-gift :powder-snow :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :secret-power :sheer-cold :signal-beam :sleep-talk :snore :spit-up :stockpile :strength :substitute :super-fang :surf :swagger :swallow :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool :yawn :charm :double-edge :icy-wind :mimic :mud-slap]
  :sealeo [:roar :double-edge :icy-wind :mimic]
  :walrein [:avalanche :block :crunch :fury-cutter :giga-impact :hyper-beam :ice-fang :iron-head :double-edge :icy-wind :mimic])

(deflearnset :bw
  :spheal [:aqua-ring :aqua-tail :attract :aurora-beam :blizzard :body-slam :bulldoze :curse :defense-curl :dive :double-team :earthquake :echoed-voice :encore :facade :fissure :frost-breath :frustration :growl :hail :hidden-power :ice-ball :ice-beam :icy-wind :iron-tail :powder-snow :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :round :sheer-cold :signal-beam :sleep-talk :snore :spit-up :stockpile :strength :substitute :super-fang :surf :swagger :swallow :toxic :water-gun :water-pulse :water-sport :waterfall :yawn :charm :double-edge :icy-wind :mimic :mud-slap]
  :sealeo [:roar :double-edge :icy-wind :mimic]
  :walrein [:block :crunch :giga-impact :hyper-beam :ice-fang :iron-head :double-edge :icy-wind :mimic])

