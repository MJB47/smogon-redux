
(in-ns 'smogon.dex)


(defpokemon :seviper
  :name "Seviper"
  :introduced-in :rs
  :types [[:poison]]
  :egggroups [[:dragon :ground]]
  :abilities [[:shed-skin]
              :bw [:infiltrator :shed-skin]]
  :stats [[73 100 60 100 60 65]]
  :weight [52.5]
  :height [2.7])

(deffamily (familychain :seviper))

(deflearnset :rs
  :seviper [:attract :bite :body-slam :crunch :dig :double-edge :double-team :earthquake :endure :facade :flamethrower :frustration :fury-cutter :giga-drain :glare :haze :hidden-power :iron-tail :lick :mimic :mud-slap :poison-fang :poison-tail :protect :rain-dance :rest :return :rock-smash :screech :secret-power :sleep-talk :sludge-bomb :snatch :snore :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :swift :taunt :thief :toxic :wrap :double-edge :mimic])

(deflearnset :dp
  :seviper [:aqua-tail :assurance :attract :bite :body-slam :captivate :crunch :dark-pulse :dig :double-team :earthquake :endure :facade :flamethrower :frustration :fury-cutter :giga-drain :glare :haze :headbutt :hidden-power :iron-tail :knock-off :lick :mud-slap :natural-gift :night-slash :payback :poison-fang :poison-jab :poison-tail :protect :rain-dance :rest :return :rock-smash :scary-face :screech :secret-power :sleep-talk :sludge-bomb :snatch :snore :spit-up :stockpile :strength :substitute :sucker-punch :sunny-day :swagger :swallow :swift :switcheroo :taunt :thief :toxic :wrap :wring-out :x-scissor :double-edge :mimic])

(deflearnset :bw
  :seviper [:aqua-tail :assurance :attract :bind :bite :body-slam :bulldoze :coil :crunch :dark-pulse :dig :double-team :dragon-tail :earthquake :facade :final-gambit :flamethrower :frustration :gastro-acid :giga-drain :glare :haze :hidden-power :iron-tail :knock-off :lick :night-slash :payback :poison-fang :poison-jab :poison-tail :protect :punishment :rain-dance :rest :retaliate :return :rock-smash :round :scary-face :screech :sleep-talk :sludge-bomb :sludge-wave :snatch :snore :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :switcheroo :taunt :thief :toxic :venoshock :wrap :wring-out :x-scissor :double-edge :mimic])
