
(in-ns 'smogon.dex)


(defpokemon :trapinch
  :name "Trapinch"
  :introduced-in :rs
  :types [[:ground]]
  :egggroups [[:bug]]
  :abilities [[:arena-trap :hyper-cutter]
              :bw [:arena-trap :hyper-cutter :sheer-force]]
  :stats [[45 100 45 45 45 10]]
  :weight [15]
  :height [0.7])

(defpokemon :vibrava
  :name "Vibrava"
  :introduced-in :rs
  :types [[:dragon :ground]]
  :egggroups [[:bug]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[50 70 50 50 50 70]]
  :weight [15.3]
  :height [1.1])

(defpokemon :flygon
  :name "Flygon"
  :introduced-in :rs
  :types [[:dragon :ground]]
  :egggroups [[:bug]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[80 100 80 80 80 100]]
  :weight [82]
  :height [2])

(deffamily (familychain :trapinch :vibrava :flygon))



(deflearnset :rs
  :trapinch [:attract :bite :body-slam :crunch :dig :double-edge :double-team :earthquake :endure :facade :faint-attack :focus-energy :frustration :giga-drain :gust :hidden-power :hyper-beam :mimic :mud-slap :protect :quick-attack :rest :return :rock-slide :rock-smash :rock-tomb :sand-attack :sand-tomb :sandstorm :secret-power :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :toxic :body-slam :double-edge :mimic]
  :vibrava [:dragonbreath :fly :screech :steel-wing :swift :body-slam :double-edge :mimic]
  :flygon [:dragon-claw :fire-blast :fire-punch :flamethrower :fury-cutter :iron-tail :body-slam :double-edge :mimic])

(deflearnset :dp
  :trapinch [:attract :bite :bug-bite :captivate :crunch :dig :double-team :earth-power :earthquake :endure :facade :faint-attack :feint :fissure :flail :focus-energy :frustration :fury-cutter :giga-drain :gust :headbutt :hidden-power :hyper-beam :mud-shot :mud-slap :natural-gift :protect :quick-attack :rest :return :rock-slide :rock-smash :rock-tomb :sand-attack :sand-tomb :sandstorm :secret-power :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :toxic :body-slam :double-edge :mimic]
  :vibrava [:air-cutter :defog :draco-meteor :dragon-pulse :dragonbreath :fly :heat-wave :ominous-wind :outrage :roost :screech :silver-wind :sonicboom :steel-wing :supersonic :swift :tailwind :twister :u-turn :body-slam :double-edge :mimic]
  :flygon [:aerial-ace :dragon-claw :fire-blast :fire-punch :flamethrower :giga-impact :iron-tail :stone-edge :thunderpunch :body-slam :double-edge :mimic])

(deflearnset :bw
  :trapinch [:attract :bide :bite :bug-bite :bulldoze :crunch :dig :double-team :earth-power :earthquake :endure :facade :faint-attack :feint :fissure :flail :focus-energy :frustration :fury-cutter :giga-drain :gust :hidden-power :hyper-beam :mud-shot :mud-slap :protect :quick-attack :rest :return :rock-slide :rock-smash :rock-tomb :round :sand-attack :sand-tomb :sandstorm :signal-beam :sleep-talk :snore :solarbeam :strength :struggle-bug :substitute :sunny-day :superpower :swagger :toxic :body-slam :double-edge :mimic]
  :vibrava [:draco-meteor :dragon-pulse :dragonbreath :fly :heat-wave :outrage :roost :screech :sonicboom :supersonic :tailwind :u-turn :body-slam :double-edge :mimic]
  :flygon [:aerial-ace :dragon-claw :dragon-tail :fire-blast :fire-punch :flamethrower :giga-impact :hone-claws :incinerate :iron-tail :stone-edge :thunderpunch :body-slam :double-edge :mimic])

