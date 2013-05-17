
(in-ns 'smogon.dex)


(defpokemon :regirock
  :name "Regirock"
  :introduced-in :rs
  :types [[:rock]]
  :egggroups [[]]
  :abilities [[:clear-body]
              :bw [:clear-body :sturdy]]
  :stats [[80 100 200 50 100 50]]
  :weight [230]
  :height [1.7])

(deffamily (familychain :regirock))

(deflearnset :rs
  :regirock [:ancientpower :body-slam :brick-break :counter :curse :defense-curl :dig :double-edge :double-team :dynamicpunch :earthquake :endure :explosion :facade :fire-punch :focus-punch :frustration :hidden-power :hyper-beam :ice-punch :iron-defense :lock-on :mega-kick :mega-punch :mimic :mud-slap :protect :psych-up :rest :return :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :safeguard :sandstorm :secret-power :seismic-toss :selfdestruct :shock-wave :sleep-talk :snore :strength :substitute :sunny-day :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :dp
  :regirock [:ancientpower :block :brick-break :charge-beam :curse :dig :double-team :drain-punch :earth-power :earthquake :endure :explosion :facade :fire-punch :fling :focus-blast :focus-punch :frustration :giga-impact :gravity :hammer-arm :headbutt :hidden-power :hyper-beam :ice-punch :iron-defense :iron-head :lock-on :mud-slap :natural-gift :protect :psych-up :rest :return :rock-climb :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :safeguard :sandstorm :secret-power :shock-wave :sleep-talk :snore :stealth-rock :stomp :stone-edge :strength :substitute :sunny-day :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :bw
  :regirock [:ancientpower :block :brick-break :bulldoze :charge-beam :curse :dig :double-team :drain-punch :earth-power :earthquake :explosion :facade :fire-punch :fling :focus-blast :frustration :giga-impact :gravity :hammer-arm :hidden-power :hyper-beam :ice-punch :iron-defense :iron-head :lock-on :protect :psych-up :rest :return :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :round :safeguard :sandstorm :sleep-talk :smack-down :snore :stealth-rock :stomp :stone-edge :strength :substitute :sunny-day :superpower :swagger :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :zap-cannon :body-slam :double-edge :mimic :seismic-toss :selfdestruct])
