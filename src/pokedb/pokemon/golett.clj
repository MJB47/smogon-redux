
(in-ns 'smogon.dex)


(defpokemon :golett
  :name "Golett"
  :introduced-in :bw
  :types [[:ghost :ground]]
  :egggroups [[:mineral]]
  :abilities [[:iron-fist :klutz :no-guard]]
  :stats [[59 74 50 35 50 35]]
  :weight [92]
  :height [1])

(defpokemon :golurk
  :name "Golurk"
  :introduced-in :bw
  :types [[:ghost :ground]]
  :egggroups [[:mineral]]
  :abilities [[:iron-fist :klutz :no-guard]]
  :stats [[89 124 80 55 80 55]]
  :weight [330]
  :height [2.8])

(deffamily (familychain :golett :golurk))

(deflearnset :bw
  :golett [:astonish :block :brick-break :bulldoze :curse :defense-curl :double-team :drain-punch :dynamicpunch :earth-power :earthquake :facade :fire-punch :flash :fling :focus-blast :focus-punch :frustration :grass-knot :gravity :gyro-ball :hammer-arm :hidden-power :ice-beam :ice-punch :icy-wind :iron-defense :low-kick :low-sweep :magic-coat :magnitude :mega-punch :mud-slap :night-shade :pound :protect :psychic :rain-dance :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :round :safeguard :shadow-ball :shadow-punch :signal-beam :sleep-talk :snore :stealth-rock :strength :substitute :superpower :swagger :telekinesis :thief :thunderpunch :toxic]
  :golurk [:charge-beam :flash-cannon :fly :giga-impact :heavy-slam :hyper-beam :solarbeam :stone-edge :thunderbolt :zen-headbutt])
