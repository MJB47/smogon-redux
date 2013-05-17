
(in-ns 'smogon.dex)


(defpokemon :shinx
  :name "Shinx"
  :introduced-in :dp
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:guts :intimidate :rivalry]]
  :stats [[45 65 34 40 34 45]]
  :weight [9.5]
  :height [0.5])

(defpokemon :luxio
  :name "Luxio"
  :introduced-in :dp
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:guts :intimidate :rivalry]]
  :stats [[60 85 49 60 49 60]]
  :weight [30.5]
  :height [0.9])

(defpokemon :luxray
  :name "Luxray"
  :introduced-in :dp
  :types [[:electric]]
  :egggroups [[:ground]]
  :abilities [[:guts :intimidate :rivalry]]
  :stats [[80 120 79 95 79 70]]
  :weight [42]
  :height [1.4])

(deffamily (familychain :shinx :luxio :luxray))

(deflearnset :dp
  :shinx [:attract :bite :captivate :charge :charge-beam :crunch :discharge :double-team :endure :facade :fire-fang :flash :frustration :fury-cutter :headbutt :hidden-power :howl :ice-fang :iron-tail :leer :light-screen :magnet-rise :mud-slap :natural-gift :night-slash :protect :quick-attack :rain-dance :rest :return :roar :scary-face :secret-power :shock-wave :signal-beam :sleep-talk :snore :spark :strength :substitute :swagger :swift :tackle :take-down :thief :thunder :thunder-fang :thunder-wave :thunderbolt :toxic]
  :luxray [:giga-impact :hyper-beam :superpower])

(deflearnset :bw
  :shinx [:attract :bite :charge :charge-beam :crunch :discharge :double-kick :double-team :facade :fire-fang :flash :frustration :helping-hand :hidden-power :howl :ice-fang :iron-tail :leer :light-screen :magnet-rise :night-slash :protect :quick-attack :rain-dance :rest :return :roar :round :scary-face :shock-wave :signal-beam :sleep-talk :snarl :snore :spark :strength :substitute :swagger :swift :tackle :take-down :thief :thunder :thunder-fang :thunder-wave :thunderbolt :toxic :volt-switch :wild-charge]
  :luxray [:giga-impact :hyper-beam :superpower])
