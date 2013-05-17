
(in-ns 'smogon.dex)


(defpokemon :nosepass
  :name "Nosepass"
  :introduced-in :rs
  :types [[:rock]]
  :egggroups [[:mineral]]
  :abilities [[:magnet-pull :sturdy]
              :bw [:magnet-pull :sand-force :sturdy]]
  :stats [[30 45 135 45 90 30]]
  :weight [97]
  :height [1])

(defpokemon :probopass
  :name "Probopass"
  :introduced-in :dp
  :types [[:rock :steel]]
  :egggroups [[:mineral]]
  :abilities [[:magnet-pull :sand-force :sturdy]]
  :stats [[60 55 145 75 150 40]]
  :weight [340]
  :height [1.4])

(deffamily (familychain :nosepass :probopass))

(deflearnset :rs
  :nosepass [:attract :block :body-slam :defense-curl :double-edge :double-team :dynamicpunch :earthquake :endure :explosion :facade :fire-punch :frustration :harden :hidden-power :ice-punch :lock-on :magnitude :mimic :mud-slap :protect :rest :return :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :sandstorm :secret-power :selfdestruct :shock-wave :sleep-talk :snore :strength :substitute :sunny-day :swagger :tackle :taunt :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :zap-cannon :body-slam :helping-hand :mimic :selfdestruct])

(deflearnset :dp
  :nosepass [:ancientpower :attract :block :captivate :discharge :double-edge :double-team :earth-power :earthquake :endure :explosion :facade :fire-punch :frustration :gravity :harden :head-smash :headbutt :hidden-power :ice-punch :iron-defense :lock-on :magic-coat :magnet-rise :magnitude :mud-slap :natural-gift :pain-split :power-gem :protect :rest :return :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :sandstorm :secret-power :shock-wave :sleep-talk :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :tackle :taunt :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :zap-cannon :body-slam :helping-hand :mimic :selfdestruct]
  :probopass [:flash-cannon :giga-impact :hyper-beam :iron-head :magnet-bomb])

(deflearnset :bw
  :nosepass [:attract :block :bulldoze :discharge :double-edge :double-team :earth-power :earthquake :endure :explosion :facade :fire-punch :frustration :gravity :harden :hidden-power :ice-punch :iron-defense :lock-on :magic-coat :magnet-rise :magnitude :pain-split :power-gem :protect :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :round :sandstorm :sleep-talk :smack-down :snore :spark :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :tackle :taunt :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :volt-switch :zap-cannon :body-slam :helping-hand :mimic :selfdestruct]
  :probopass [:flash-cannon :giga-impact :hyper-beam :iron-head :magnet-bomb])
