
(in-ns 'smogon.dex)


(defpokemon :larvitar
  :name "Larvitar"
  :introduced-in :gs
  :types [[:ground :rock]]
  :egggroups [[:monster]]
  :abilities [[:guts]
              :bw [:guts :sand-veil]]
  :stats [[50 64 50 45 50 41]]
  :weight [72]
  :height [0.6])

(defpokemon :pupitar
  :name "Pupitar"
  :introduced-in :gs
  :types [[:ground :rock]]
  :egggroups [[:monster]]
  :abilities [[:shed-skin]]
  :stats [[70 84 70 65 70 51]]
  :weight [152]
  :height [1.2])

(defpokemon :tyranitar
  :name "Tyranitar"
  :introduced-in :gs
  :types [[:dark :rock]]
  :egggroups [[:monster]]
  :abilities [[:sand-stream]
              :bw [:sand-stream :unnerve]]
  :stats [[100 134 110 95 100 61]]
  :weight [202]
  :height [2])

(deffamily (familychain :larvitar :pupitar :tyranitar))
(deflearnset :gs
  :larvitar [:ancientpower :attract :bite :crunch :curse :detect :dig :double-team :earthquake :endure :focus-energy :frustration :headbutt :hidden-power :hyper-beam :leer :mud-slap :outrage :protect :pursuit :rain-dance :rest :return :rock-slide :sandstorm :scary-face :screech :sleep-talk :snore :stomp :sunny-day :swagger :thrash :toxic]
  :pupitar []
  :tyranitar [:cut :dragonbreath :dynamicpunch :fire-blast :fire-punch :flamethrower :fury-cutter :ice-beam :iron-tail :nightmare :roar :rock-smash :strength :surf :thunderbolt])

(deflearnset :rs
  :larvitar [:ancientpower :attract :bite :body-slam :brick-break :crunch :curse :dig :double-edge :double-team :dragon-dance :earthquake :endure :facade :focus-energy :frustration :hidden-power :hyper-beam :leer :mimic :mud-slap :outrage :protect :pursuit :rain-dance :rest :return :rock-slide :rock-smash :sandstorm :scary-face :screech :secret-power :sleep-talk :snore :stomp :substitute :sunny-day :swagger :taunt :thrash :torment :toxic :body-slam :double-edge :mimic]
  :pupitar [ :body-slam :double-edge :mimic]
  :tyranitar [:aerial-ace :blizzard :counter :cut :dragon-claw :dynamicpunch :fire-blast :fire-punch :flamethrower :focus-punch :fury-cutter :ice-beam :iron-tail :mega-kick :mega-punch :nightmare :roar :rock-tomb :seismic-toss :shock-wave :strength :surf :thunder :thunder-wave :thunderbolt :water-pulse :body-slam :double-edge :mimic :nightmare :seismic-toss])

(deflearnset :dp
  :larvitar [:ancientpower :assurance :attract :bite :brick-break :captivate :crunch :curse :dark-pulse :dig :double-team :dragon-dance :earth-power :earthquake :endure :facade :focus-energy :frustration :headbutt :hidden-power :hyper-beam :iron-defense :iron-head :leer :mud-slap :natural-gift :outrage :payback :protect :pursuit :rain-dance :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :sandstorm :scary-face :screech :secret-power :sleep-talk :snore :spite :stealth-rock :stomp :stone-edge :substitute :sunny-day :superpower :swagger :taunt :thrash :torment :toxic :uproar :body-slam :double-edge :mimic]
  :pupitar [ :body-slam :double-edge :mimic]
  :tyranitar [:aerial-ace :aqua-tail :avalanche :blizzard :block :cut :dragon-claw :dragon-pulse :fire-blast :fire-fang :fire-punch :flamethrower :fling :focus-blast :focus-punch :fury-cutter :giga-impact :ice-beam :ice-fang :ice-punch :iron-tail :low-kick :roar :rock-climb :shadow-claw :shock-wave :strength :surf :thunder :thunder-fang :thunder-wave :thunderbolt :thunderpunch :water-pulse :whirlpool :body-slam :double-edge :mimic :nightmare :seismic-toss])

(deflearnset :bw
  :larvitar [:ancientpower :assurance :attract :bite :brick-break :bulldoze :chip-away :crunch :curse :dark-pulse :dig :double-team :dragon-dance :earth-power :earthquake :facade :focus-energy :frustration :hidden-power :hyper-beam :iron-defense :iron-head :iron-tail :leer :outrage :payback :protect :pursuit :rain-dance :rest :retaliate :return :rock-polish :rock-slide :rock-smash :rock-tomb :round :sandstorm :scary-face :screech :sleep-talk :smack-down :snarl :snore :spite :stealth-rock :stomp :stone-edge :substitute :sunny-day :superpower :swagger :taunt :thrash :torment :toxic :uproar :body-slam :double-edge :mimic]
  :pupitar [ :body-slam :double-edge :mimic]
  :tyranitar [:aerial-ace :aqua-tail :blizzard :block :cut :dragon-claw :dragon-pulse :dragon-tail :fire-blast :fire-fang :fire-punch :flamethrower :fling :focus-blast :foul-play :giga-impact :hone-claws :ice-beam :ice-fang :ice-punch :incinerate :low-kick :roar :shadow-claw :strength :surf :thunder :thunder-fang :thunder-wave :thunderbolt :thunderpunch :body-slam :double-edge :mimic :nightmare :seismic-toss])
