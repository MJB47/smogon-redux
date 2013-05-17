
(in-ns 'smogon.dex)


(defpokemon :cranidos
  :name "Cranidos"
  :introduced-in :dp
  :types [[:rock]]
  :egggroups [[:monster]]
  :abilities [[:mold-breaker :sheer-force]]
  :stats [[67 125 40 30 30 58]]
  :weight [31.5]
  :height [0.9])

(defpokemon :rampardos
  :name "Rampardos"
  :introduced-in :dp
  :types [[:rock]]
  :egggroups [[:monster]]
  :abilities [[:mold-breaker :sheer-force]]
  :stats [[97 165 60 65 50 58]]
  :weight [102.5]
  :height [1.6])

(deffamily (familychain :cranidos :rampardos))

(deflearnset :dp
  :cranidos [:ancientpower :assurance :attract :blizzard :captivate :crunch :curse :dig :double-edge :double-team :dragon-pulse :earth-power :earthquake :endeavor :endure :facade :fire-blast :fire-punch :flamethrower :fling :focus-energy :frustration :hammer-arm :head-smash :headbutt :hidden-power :ice-beam :iron-head :iron-tail :leer :mud-slap :natural-gift :payback :protect :pursuit :rain-dance :rest :return :roar :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :sandstorm :scary-face :screech :secret-power :shock-wave :slam :sleep-talk :snore :spite :stealth-rock :stomp :stone-edge :strength :substitute :sunny-day :superpower :swagger :swords-dance :take-down :thief :thrash :thunder :thunderbolt :thunderpunch :toxic :uproar :whirlwind :zen-headbutt]
  :rampardos [:avalanche :brick-break :cut :focus-blast :focus-punch :giga-impact :hyper-beam :outrage :pain-split :surf :whirlpool])

(deflearnset :bw
  :cranidos [:ancientpower :assurance :attract :blizzard :bulldoze :chip-away :crunch :curse :dig :double-edge :double-team :dragon-pulse :earth-power :earthquake :endeavor :facade :fire-blast :fire-punch :flamethrower :fling :focus-energy :frustration :hammer-arm :head-smash :headbutt :hidden-power :ice-beam :incinerate :iron-head :iron-tail :leer :payback :protect :pursuit :rain-dance :rest :return :roar :rock-polish :rock-slide :rock-smash :rock-tomb :round :sandstorm :scary-face :screech :slam :sleep-talk :smack-down :snore :spite :stealth-rock :stomp :stone-edge :strength :substitute :sunny-day :superpower :swagger :swords-dance :take-down :thief :thrash :thunder :thunderbolt :thunderpunch :toxic :uproar :whirlwind :zen-headbutt]
  :rampardos [:brick-break :cut :dragon-tail :focus-blast :giga-impact :hyper-beam :outrage :pain-split :surf])
