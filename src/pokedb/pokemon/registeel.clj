
(in-ns 'smogon.dex)


(defpokemon :registeel
  :name "Registeel"
  :introduced-in :rs
  :types [[:steel]]
  :egggroups [[]]
  :abilities [[:clear-body]
              :bw [:clear-body :light-metal]]
  :stats [[80 75 150 75 150 50]]
  :weight [205]
  :height [1.9])

(deffamily (familychain :registeel))

(deflearnset :rs
  :registeel [:aerial-ace :amnesia :ancientpower :body-slam :brick-break :counter :curse :defense-curl :double-edge :double-team :dynamicpunch :earthquake :endure :explosion :facade :focus-punch :frustration :hidden-power :hyper-beam :ice-punch :iron-defense :lock-on :mega-kick :mega-punch :metal-claw :mimic :mud-slap :protect :psych-up :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sandstorm :secret-power :seismic-toss :selfdestruct :shock-wave :sleep-talk :snore :strength :substitute :sunny-day :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :dp
  :registeel [:aerial-ace :amnesia :ancientpower :block :brick-break :charge-beam :curse :double-team :earthquake :endure :explosion :facade :flash-cannon :fling :focus-blast :focus-punch :frustration :giga-impact :gravity :hammer-arm :headbutt :hidden-power :hyper-beam :ice-punch :iron-defense :iron-head :lock-on :magnet-rise :metal-claw :mud-slap :natural-gift :protect :psych-up :rain-dance :rest :return :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sandstorm :secret-power :shadow-claw :shock-wave :sleep-talk :snore :stealth-rock :stomp :strength :substitute :sunny-day :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :bw
  :registeel [:aerial-ace :amnesia :ancientpower :block :brick-break :bulldoze :charge-beam :curse :double-team :earthquake :explosion :facade :flash-cannon :fling :focus-blast :frustration :giga-impact :gravity :hammer-arm :hidden-power :hone-claws :hyper-beam :ice-punch :iron-defense :iron-head :lock-on :magnet-rise :metal-claw :protect :psych-up :rain-dance :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :round :safeguard :sandstorm :shadow-claw :sleep-talk :snore :stealth-rock :stomp :strength :substitute :sunny-day :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])
