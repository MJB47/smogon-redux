
(in-ns 'smogon.dex)

(defpokemon :aron
  :name "Aron"
  :introduced-in :rs
  :types [[:rock :steel]]
  :egggroups [[:monster]]
  :abilities [[:rock-head :sturdy]
              :bw [:heavy-metal :rock-head :sturdy]]
  :stats [[50 70 100 40 40 30]]
  :weight [60]
  :height [0.4])

(defpokemon :lairon
  :name "Lairon"
  :introduced-in :rs
  :types [[:rock :steel]]
  :egggroups [[:monster]]
  :abilities [[:rock-head :sturdy]
              :bw [:heavy-metal :rock-head :sturdy]]
  :stats [[60 90 140 50 50 40]]
  :weight [120]
  :height [0.9])

(defpokemon :aggron
  :name "Aggron"
  :introduced-in :rs
  :types [[:rock :steel]]
  :egggroups [[:monster]]
  :abilities [[:rock-head :sturdy]
              :bw [:heavy-metal :rock-head :sturdy]]
  :stats [[70 110 180 60 60 50]]
  :weight [360]
  :height [2.1])

(deffamily (familychain :aron :lairon :aggron))

(deflearnset :rs
  :aron [:aerial-ace :attract :body-slam :cut :defense-curl :dig :double-edge :double-team :earthquake :endeavor :endure :facade :frustration :fury-cutter :harden :headbutt :hidden-power :iron-defense :iron-tail :metal-claw :metal-sound :mimic :mud-slap :protect :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :secret-power :shock-wave :sleep-talk :smellingsalt :snore :stomp :strength :substitute :sunny-day :swagger :tackle :take-down :toxic :water-pulse]
  :lairon [ :mimic]
  :aggron [:blizzard :brick-break :counter :dragon-claw :dynamicpunch :fire-blast :fire-punch :flamethrower :focus-punch :hyper-beam :ice-beam :ice-punch :icy-wind :mega-kick :mega-punch :seismic-toss :solarbeam :surf :taunt :thunder :thunder-wave :thunderbolt :thunderpunch])

(deflearnset :dp
  :aron [:aerial-ace :ancientpower :attract :body-slam :captivate :curse :cut :dig :double-edge :double-team :dragon-rush :earth-power :earthquake :endeavor :endure :facade :frustration :fury-cutter :harden :head-smash :headbutt :hidden-power :iron-defense :iron-head :iron-tail :magnet-rise :metal-burst :metal-claw :metal-sound :mud-slap :natural-gift :protect :rain-dance :rest :return :roar :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :screech :secret-power :shadow-claw :shock-wave :sleep-talk :smellingsalt :snore :spite :stealth-rock :stomp :strength :substitute :sunny-day :superpower :swagger :tackle :take-down :toxic :uproar :water-pulse]
  :lairon [:stone-edge :mimic]
  :aggron [:aqua-tail :avalanche :blizzard :block :brick-break :dark-pulse :dragon-claw :dragon-pulse :fire-blast :fire-punch :flamethrower :flash-cannon :fling :focus-blast :focus-punch :giga-impact :hyper-beam :ice-beam :ice-punch :icy-wind :low-kick :outrage :payback :rock-climb :solarbeam :surf :taunt :thunder :thunder-wave :thunderbolt :thunderpunch :whirlpool])

(deflearnset :bw
  :aron [:aerial-ace :attract :autotomize :body-slam :bulldoze :curse :cut :dig :double-edge :double-team :dragon-rush :earth-power :earthquake :endeavor :facade :frustration :harden :head-smash :headbutt :heavy-slam :hidden-power :hone-claws :iron-defense :iron-head :iron-tail :magnet-rise :metal-burst :metal-claw :metal-sound :mimic :mud-slap :protect :rain-dance :rest :return :roar :rock-polish :rock-slide :rock-smash :rock-tomb :round :sandstorm :screech :shadow-claw :shock-wave :sleep-talk :smellingsalt :snore :spite :stealth-rock :stomp :strength :substitute :sunny-day :superpower :swagger :tackle :take-down :toxic :uproar]
  :lairon [:stone-edge :mimic]
  :aggron [:aqua-tail :blizzard :block :brick-break :dark-pulse :dragon-claw :dragon-pulse :dragon-tail :fire-blast :fire-punch :flamethrower :flash-cannon :fling :focus-blast :giga-impact :hyper-beam :ice-beam :ice-punch :icy-wind :incinerate :low-kick :outrage :payback :seismic-toss :smack-down :solarbeam :surf :taunt :thunder :thunder-wave :thunderbolt :thunderpunch])
