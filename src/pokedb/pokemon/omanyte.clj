
(in-ns 'smogon.dex)


(defpokemon :omanyte
  :name "Omanyte"
  :introduced-in :rb
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:shell-armor :swift-swim]
              :bw [:shell-armor :swift-swim :weak-armor]]
  :stats [[35 40 100 90 35]
          :gs [35 40 100 90 55 35]]
  :weight [7.5]
  :height [0.4])

(defpokemon :omastar
  :name "Omastar"
  :introduced-in :rb
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:shell-armor :swift-swim]
              :bw [:shell-armor :swift-swim :weak-armor]]
  :stats [[70 60 125 115 55]
          :gs [70 60 125 115 70 55]]
  :weight [35]
  :height [1])

(deffamily (familychain :omanyte :omastar))

(deflearnset :rb
  :omanyte [:bide :blizzard :body-slam :bubblebeam :double-edge :double-team :horn-attack :hydro-pump :ice-beam :leer :mimic :rage :reflect :rest :spike-cannon :substitute :surf :take-down :toxic :water-gun :withdraw]
  :omastar [:horn-drill :hyper-beam :seismic-toss :skull-bash :submission])

(deflearnset :gs
  :omanyte [:ancientpower :attract :aurora-beam :bite :blizzard :bubblebeam :constrict :curse :double-team :endure :frustration :haze :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :leer :protect :rain-dance :rest :return :rock-smash :rollout :sandstorm :slam :sleep-talk :snore :supersonic :surf :swagger :thief :toxic :water-gun :whirlpool :withdraw]
  :omastar [:hyper-beam :spike-cannon])

(deflearnset :rs
  :omanyte [:ancientpower :attract :aurora-beam :bite :blizzard :body-slam :bubblebeam :constrict :dive :double-edge :double-team :endure :facade :frustration :hail :haze :hidden-power :hydro-pump :ice-beam :icy-wind :leer :mimic :mud-shot :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :secret-power :slam :sleep-talk :snore :spikes :substitute :supersonic :surf :swagger :thief :tickle :toxic :water-gun :water-pulse :waterfall :withdraw :body-slam :double-edge :icy-wind :mimic]
  :omastar [:hyper-beam :seismic-toss :spike-cannon :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :omanyte [:ancientpower :attract :aurora-beam :bite :blizzard :brine :bubblebeam :captivate :constrict :dive :double-team :earth-power :endure :facade :frustration :gyro-ball :hail :haze :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :iron-defense :knock-off :leer :mud-shot :muddy-water :natural-gift :protect :rain-dance :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :secret-power :slam :sleep-talk :snore :spikes :stealth-rock :substitute :supersonic :surf :swagger :thief :tickle :toxic :toxic-spikes :water-gun :water-pulse :waterfall :whirlpool :withdraw :wring-out :body-slam :double-edge :icy-wind :mimic]
  :omastar [:giga-impact :hyper-beam :rock-climb :spike-cannon :stone-edge :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :omanyte [:ancientpower :attract :aurora-beam :bide :bind :bite :blizzard :brine :bubblebeam :constrict :dive :double-team :earth-power :facade :frustration :gyro-ball :hail :haze :hidden-power :hydro-pump :ice-beam :icy-wind :iron-defense :knock-off :leer :mud-shot :muddy-water :protect :rain-dance :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :round :sandstorm :scald :shell-smash :slam :sleep-talk :smack-down :snore :spikes :stealth-rock :substitute :supersonic :surf :swagger :thief :tickle :toxic :toxic-spikes :water-gun :water-pulse :waterfall :whirlpool :withdraw :wring-out :body-slam :double-edge :icy-wind :mimic]
  :omastar [:giga-impact :hyper-beam :spike-cannon :stone-edge :body-slam :double-edge :icy-wind :mimic :seismic-toss])
