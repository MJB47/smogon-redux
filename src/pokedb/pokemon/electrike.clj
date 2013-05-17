
(in-ns 'smogon.dex)


(defpokemon :electrike
  :name "Electrike"
  :introduced-in :rs
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:lightningrod :static]
              :bw [:lightningrod :minus :static]]
  :stats [[40 45 40 65 40 65]]
  :weight [15.2]
  :height [0.6])

(defpokemon :manectric
  :name "Manectric"
  :introduced-in :rs
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:lightningrod :static]
              :bw [:lightningrod :minus :static]]
  :stats [[70 75 60 105 60 105]]
  :weight [40.2]
  :height [1.5])

(deffamily (familychain :electrike :manectric))

(deflearnset :rs
  :electrike [:attract :bite :body-slam :charge :crunch :curse :double-edge :double-team :endure :facade :flash :frustration :headbutt :hidden-power :howl :iron-tail :leer :mimic :mud-slap :odor-sleuth :protect :quick-attack :rain-dance :rest :return :roar :secret-power :shock-wave :sleep-talk :snore :spark :strength :substitute :swagger :swift :tackle :thief :thunder :thunder-wave :thunderbolt :toxic :uproar :body-slam :double-edge :mimic]
  :manectric [:hyper-beam :body-slam :double-edge :mimic :refresh])

(deflearnset :dp
  :electrike [:attract :bite :captivate :charge :charge-beam :crunch :curse :discharge :double-team :endure :facade :fire-fang :flamethrower :flash :frustration :headbutt :hidden-power :howl :ice-fang :iron-tail :leer :light-screen :magnet-rise :mud-slap :natural-gift :odor-sleuth :protect :quick-attack :rain-dance :rest :return :roar :secret-power :shock-wave :signal-beam :sleep-talk :snore :spark :strength :substitute :swagger :swift :switcheroo :tackle :thief :thunder :thunder-fang :thunder-wave :thunderbolt :toxic :uproar :body-slam :double-edge :mimic]
  :manectric [:giga-impact :hyper-beam :overheat :body-slam :double-edge :mimic :refresh])

(deflearnset :bw
  :electrike [:attract :bite :charge :charge-beam :crunch :curse :discharge :double-team :electro-ball :facade :fire-fang :flame-burst :flamethrower :flash :frustration :headbutt :hidden-power :howl :ice-fang :iron-tail :leer :light-screen :magnet-rise :odor-sleuth :protect :quick-attack :rain-dance :rest :return :roar :round :shock-wave :signal-beam :sleep-talk :snarl :snore :spark :strength :substitute :swagger :swift :switcheroo :tackle :thief :thunder :thunder-fang :thunder-wave :thunderbolt :toxic :uproar :volt-switch :wild-charge :body-slam :double-edge :mimic]
  :manectric [:giga-impact :hyper-beam :overheat :body-slam :double-edge :mimic :refresh])
