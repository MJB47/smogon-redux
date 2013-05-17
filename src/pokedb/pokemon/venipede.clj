
(in-ns 'smogon.dex)


(defpokemon :venipede
  :name "Venipede"
  :introduced-in :bw
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:poison-point :quick-feet :swarm]]
  :stats [[30 45 59 30 39 57]]
  :weight [5.3]
  :height [0.4])

(defpokemon :whirlipede
  :name "Whirlipede"
  :introduced-in :bw
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:poison-point :quick-feet :swarm]]
  :stats [[40 55 99 40 79 47]]
  :weight [58.5]
  :height [1.2])

(defpokemon :scolipede
  :name "Scolipede"
  :introduced-in :bw
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:poison-point :quick-feet :swarm]]
  :stats [[60 90 89 55 69 112]]
  :weight [200.5]
  :height [2.5])

(deffamily (familychain :venipede :whirlipede :scolipede))



(deflearnset :bw
  :venipede [:agility :attract :bug-bite :defense-curl :double-edge :double-team :endeavor :facade :frustration :gyro-ball :hidden-power :iron-defense :payback :pin-missile :poison-jab :poison-sting :poison-tail :protect :pursuit :rest :return :rock-climb :rock-smash :rollout :round :screech :sleep-talk :sludge-bomb :snore :solarbeam :spikes :steamroller :struggle-bug :substitute :sunny-day :swagger :take-down :toxic :toxic-spikes :twineedle :venoshock]
  :scolipede [:aqua-tail :baton-pass :bulldoze :cut :dig :earthquake :giga-impact :hyper-beam :iron-tail :megahorn :rock-slide :rock-tomb :snatch :strength :superpower :swords-dance :x-scissor])

