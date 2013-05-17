
(in-ns 'smogon.dex)


(defpokemon :shelmet
  :name "Shelmet"
  :introduced-in :bw
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:hydration :overcoat :shell-armor]]
  :stats [[50 40 85 40 65 25]]
  :weight [7.7]
  :height [0.4])

(defpokemon :accelgor
  :name "Accelgor"
  :introduced-in :bw
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:hydration :sticky-hold :unburden]]
  :stats [[80 70 40 100 60 145]]
  :weight [25.3]
  :height [0.8])

(deffamily (familychain :shelmet :accelgor))

(deflearnset :bw
  :shelmet [:acid :acid-armor :attract :baton-pass :bide :body-slam :bug-bite :bug-buzz :curse :double-edge :double-team :encore :endure :energy-ball :facade :feint :final-gambit :frustration :gastro-acid :giga-drain :guard-split :guard-swap :hidden-power :leech-life :mega-drain :mind-reader :mud-slap :protect :pursuit :rain-dance :recover :rest :return :round :signal-beam :sleep-talk :sludge-bomb :snore :spikes :struggle-bug :substitute :swagger :toxic :venoshock :yawn]
  :accelgor [:acid-spray :agility :focus-blast :giga-impact :hyper-beam :knock-off :me-first :power-swap :quick-attack :sandstorm :swift :u-turn])
