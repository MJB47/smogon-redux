
(in-ns 'smogon.dex)


(defpokemon :tynamo
  :name "Tynamo"
  :introduced-in :bw
  :types [[:electric]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[35 55 40 45 40 60]]
  :weight [0.3]
  :height [0.2])

(defpokemon :eelektrik
  :name "Eelektrik"
  :introduced-in :bw
  :types [[:electric]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[65 85 70 75 70 40]]
  :weight [22]
  :height [1.2])

(defpokemon :eelektross
  :name "Eelektross"
  :introduced-in :bw
  :types [[:electric]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[85 115 80 105 80 50]]
  :weight [80.5]
  :height [2.1])

(deffamily (familychain :tynamo :eelektrik :eelektross))



(deflearnset :bw
  :tynamo [:charge-beam :magnet-rise :spark :tackle :thunder-wave]
  :eelektrik [:acid :acid-spray :acrobatics :aqua-tail :attract :bind :bounce :coil :crunch :discharge :double-team :facade :flash :flash-cannon :frustration :gastro-acid :giga-drain :headbutt :hidden-power :iron-tail :knock-off :light-screen :protect :rain-dance :rest :return :round :signal-beam :sleep-talk :snore :substitute :super-fang :swagger :thrash :thunder :thunderbolt :toxic :u-turn :volt-switch :wild-charge :zap-cannon]
  :eelektross [:brick-break :crush-claw :cut :dragon-claw :dragon-tail :drain-punch :fire-punch :flamethrower :giga-impact :grass-knot :hone-claws :hyper-beam :roar :rock-slide :rock-smash :rock-tomb :strength :superpower :thunderpunch])

