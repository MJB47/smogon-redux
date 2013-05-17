
(in-ns 'smogon.dex)


(defpokemon :klink
  :name "Klink"
  :introduced-in :bw
  :types [[:steel]]
  :egggroups [[:mineral]]
  :abilities [[:clear-body :minus :plus]]
  :stats [[40 55 70 45 60 30]]
  :weight [21]
  :height [0.3])

(defpokemon :klang
  :name "Klang"
  :introduced-in :bw
  :types [[:steel]]
  :egggroups [[:mineral]]
  :abilities [[:clear-body :minus :plus]]
  :stats [[60 80 95 70 85 50]]
  :weight [51]
  :height [0.6])

(defpokemon :klinklang
  :name "Klinklang"
  :introduced-in :bw
  :types [[:steel]]
  :egggroups [[:mineral]]
  :abilities [[:clear-body :minus :plus]]
  :stats [[60 100 115 70 85 90]]
  :weight [81]
  :height [0.6])

(deffamily (familychain :klink :klang :klinklang))

(deflearnset :bw
  :klink [:autotomize :bind :charge :charge-beam :discharge :double-team :facade :flash-cannon :frustration :gear-grind :gravity :hidden-power :hyper-beam :iron-defense :lock-on :magic-coat :magnet-rise :metal-sound :mirror-shot :protect :recycle :rest :return :rock-polish :rock-smash :round :sandstorm :screech :shift-gear :signal-beam :sleep-talk :snore :substitute :swagger :thunder-wave :thunderbolt :thundershock :toxic :uproar :vicegrip :volt-switch :wild-charge :zap-cannon]
  :klinklang [:giga-impact :thunder :trick-room])
