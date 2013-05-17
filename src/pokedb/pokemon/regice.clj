
(in-ns 'smogon.dex)


(defpokemon :regice
  :name "Regice"
  :introduced-in :rs
  :types [[:ice]]
  :egggroups [[]]
  :abilities [[:clear-body]
              :bw [:clear-body :ice-body]]
  :stats [[80 50 100 100 200 50]]
  :weight [175]
  :height [1.8])

(deffamily (familychain :regice))

(deflearnset :rs
  :regice [:amnesia :ancientpower :blizzard :body-slam :brick-break :counter :curse :defense-curl :double-edge :double-team :dynamicpunch :earthquake :endure :explosion :facade :focus-punch :frustration :hail :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :lock-on :mega-kick :mega-punch :mimic :mud-slap :protect :psych-up :rain-dance :rest :return :rock-slide :rock-smash :rollout :safeguard :secret-power :seismic-toss :selfdestruct :shock-wave :sleep-talk :snore :strength :substitute :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :dp
  :regice [:amnesia :ancientpower :avalanche :blizzard :block :brick-break :charge-beam :curse :double-team :earthquake :endure :explosion :facade :flash-cannon :fling :focus-blast :focus-punch :frustration :giga-impact :gravity :hail :hammer-arm :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-head :lock-on :mud-slap :natural-gift :protect :psych-up :rain-dance :rest :return :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :safeguard :secret-power :shock-wave :signal-beam :sleep-talk :snore :stomp :strength :substitute :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :bw
  :regice [:amnesia :ancientpower :blizzard :block :brick-break :bulldoze :charge-beam :curse :double-team :earthquake :explosion :facade :flash-cannon :fling :focus-blast :frost-breath :frustration :giga-impact :gravity :hail :hammer-arm :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-head :lock-on :protect :psych-up :rain-dance :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :round :safeguard :signal-beam :sleep-talk :snore :stomp :strength :substitute :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])
