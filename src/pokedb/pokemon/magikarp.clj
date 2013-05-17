
(in-ns 'smogon.dex)

(defpokemon :magikarp
  :name "Magikarp"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:dragon :water-2]]
  :abilities [[:swift-swim]
              :bw [:rattled :swift-swim]]
  :stats [[20 10 55 20 80]
          :gs [20 10 55 15 20 80]]
  :weight [10]
  :height [0.9])

(defpokemon :gyarados
  :name "Gyarados"
  :introduced-in :rb
  :types [[:flying :water]]
  :egggroups [[:dragon :water-2]]
  :abilities [[:intimidate]
              :bw [:intimidate :moxie]]
  :stats [[95 125 79 100 81]
          :gs [95 125 79 60 100 81]]
  :weight [235]
  :height [6.5])

(deffamily (familychain :magikarp :gyarados))

(deflearnset :rb
  :magikarp [:splash :tackle]
  :gyarados [:bide :bite :blizzard :body-slam :bubblebeam :double-edge :double-team :dragon-rage :fire-blast :hydro-pump :hyper-beam :ice-beam :leer :mimic :rage :reflect :rest :skull-bash :strength :substitute :surf :take-down :thunder :thunderbolt :toxic :water-gun])

(deflearnset :gs
  :magikarp [:flail :splash :tackle]
  :gyarados [:attract :bite :blizzard :curse :double-team :dragon-rage :dragonbreath :endure :fire-blast :flamethrower :frustration :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :leer :protect :rain-dance :rest :return :roar :rock-smash :sandstorm :sleep-talk :snore :strength :surf :swagger :thrash :thunder :thunderbolt :toxic :twister :waterfall :whirlpool :zap-cannon])

(deflearnset :rs
  :magikarp [:flail :splash :tackle]
  :gyarados [:attract :bite :blizzard :body-slam :dive :double-edge :double-team :dragon-dance :dragon-rage :earthquake :endure :facade :fire-blast :flamethrower :frustration :hail :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :leer :mimic :protect :rain-dance :rest :return :roar :rock-smash :sandstorm :secret-power :sleep-talk :snore :strength :substitute :surf :swagger :taunt :thrash :thunder :thunder-wave :thunderbolt :torment :toxic :twister :water-pulse :waterfall])

(deflearnset :dp
  :magikarp [:bounce :flail :splash :tackle]
  :gyarados [:aqua-tail :attract :avalanche :bite :blizzard :brine :captivate :dark-pulse :dive :double-team :dragon-dance :dragon-pulse :dragon-rage :earthquake :endure :facade :fire-blast :flamethrower :frustration :giga-impact :hail :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :ice-fang :icy-wind :iron-head :leer :natural-gift :outrage :payback :protect :rain-dance :rest :return :roar :rock-smash :sandstorm :secret-power :sleep-talk :snore :spite :stone-edge :strength :substitute :surf :swagger :taunt :thrash :thunder :thunder-wave :thunderbolt :torment :toxic :twister :uproar :water-pulse :waterfall :whirlpool])

(deflearnset :bw
  :magikarp [:bounce :flail :hydro-pump :splash :tackle]
  :gyarados [:aqua-tail :attract :bite :blizzard :bulldoze :dark-pulse :dive :double-team :dragon-dance :dragon-pulse :dragon-rage :dragon-tail :earthquake :facade :fire-blast :flamethrower :frustration :giga-impact :hail :hidden-power :hyper-beam :ice-beam :ice-fang :icy-wind :incinerate :iron-head :iron-tail :leer :outrage :payback :protect :rain-dance :rest :return :roar :rock-smash :round :sandstorm :scald :sleep-talk :snore :spite :stone-edge :strength :substitute :surf :swagger :taunt :thrash :thunder :thunder-wave :thunderbolt :torment :toxic :twister :uproar :waterfall])
