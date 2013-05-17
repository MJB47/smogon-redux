
(in-ns 'smogon.dex)


(defpokemon :miltank
  :name "Miltank"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:thick-fat]
              :dp [:scrappy :thick-fat]
              :bw [:sap-sipper :scrappy :thick-fat]]
  :stats [[95 80 105 40 70 100]]
  :weight [75.5]
  :height [1.2])

(deffamily (familychain :miltank))

(deflearnset :gs
  :miltank [:attract :bide :blizzard :body-slam :curse :defense-curl :double-team :dynamicpunch :earthquake :endure :fire-punch :frustration :growl :headbutt :heal-bell :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :milk-drink :mud-slap :present :protect :psych-up :rain-dance :rest :return :reversal :rock-smash :rollout :sandstorm :seismic-toss :shadow-ball :sleep-talk :snore :stomp :strength :sunny-day :surf :swagger :sweet-scent :tackle :thunder :thunderbolt :thunderpunch :toxic :zap-cannon])

(deflearnset :rs
  :miltank [:attract :bide :blizzard :body-slam :brick-break :counter :curse :defense-curl :double-edge :double-team :dynamicpunch :earthquake :endure :facade :fire-punch :focus-punch :frustration :growl :heal-bell :helping-hand :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :mega-kick :mega-punch :metronome :milk-drink :mimic :mud-slap :present :protect :psych-up :rain-dance :rest :return :reversal :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :secret-power :seismic-toss :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :stomp :strength :substitute :sunny-day :surf :swagger :tackle :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :water-pulse :icy-wind :mimic])

(deflearnset :dp
  :miltank [:attract :bide :blizzard :block :body-slam :brick-break :captivate :curse :defense-curl :dizzy-punch :double-edge :double-team :earthquake :endure :facade :fire-punch :fling :focus-blast :focus-punch :frustration :giga-impact :growl :gyro-ball :hammer-arm :headbutt :heal-bell :helping-hand :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-head :iron-tail :milk-drink :mud-slap :natural-gift :present :protect :psych-up :punishment :rain-dance :rest :return :reversal :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :secret-power :seismic-toss :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :stealth-rock :stomp :strength :substitute :sunny-day :surf :swagger :tackle :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :wake-up-slap :water-pulse :whirlpool :zen-headbutt :icy-wind :mimic])

(deflearnset :bw
  :miltank [:after-you :attract :bide :blizzard :block :body-slam :brick-break :bulldoze :captivate :curse :defense-curl :dizzy-punch :double-edge :double-team :earthquake :echoed-voice :endure :facade :fire-punch :fling :focus-blast :frustration :giga-impact :growl :gyro-ball :hammer-arm :heal-bell :heart-stamp :helping-hand :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-head :iron-tail :milk-drink :natural-gift :present :protect :psych-up :punishment :rain-dance :rest :retaliate :return :reversal :rock-slide :rock-smash :rock-tomb :rollout :round :sandstorm :seismic-toss :shadow-ball :sleep-talk :snore :solarbeam :stealth-rock :stomp :strength :substitute :sunny-day :surf :swagger :tackle :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :wake-up-slap :work-up :zen-headbutt :icy-wind :mimic])
