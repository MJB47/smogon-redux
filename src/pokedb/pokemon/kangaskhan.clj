
(in-ns 'smogon.dex)


(defpokemon :kangaskhan
  :name "Kangaskhan"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:monster]]
  :abilities [[:early-bird]
              :dp [:early-bird :scrappy]
              :bw [:early-bird :inner-focus :scrappy]]
  :stats [[105 95 80 40 90]
          :gs [105 95 80 40 80 90]]
  :weight [80]
  :height [2.2])

(deffamily (familychain :kangaskhan))

(deflearnset :rb
  :kangaskhan [:bide :bite :blizzard :body-slam :bubblebeam :comet-punch :counter :dizzy-punch :double-edge :double-team :earthquake :fire-blast :fissure :hyper-beam :ice-beam :leer :mega-kick :mega-punch :mimic :rage :rest :rock-slide :seismic-toss :skull-bash :strength :submission :substitute :surf :tail-whip :take-down :thunder :thunderbolt :toxic :water-gun])

(deflearnset :gs
  :kangaskhan [:attract :bite :blizzard :comet-punch :curse :disable :dizzy-punch :double-team :dynamicpunch :earthquake :endure :fire-blast :fire-punch :flamethrower :focus-energy :foresight :frustration :fury-cutter :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :leer :mega-punch :mud-slap :protect :rage :rain-dance :rest :return :reversal :roar :rock-smash :safeguard :sandstorm :shadow-ball :sleep-talk :snore :stomp :strength :sunny-day :surf :swagger :tail-whip :thunder :thunderbolt :thunderpunch :toxic :zap-cannon])

(deflearnset :rs
  :kangaskhan [:aerial-ace :attract :bite :blizzard :body-slam :brick-break :comet-punch :counter :crush-claw :cut :dig :disable :dizzy-punch :double-edge :double-team :dynamicpunch :earthquake :endure :facade :fake-out :fire-blast :fire-punch :flamethrower :focus-energy :focus-punch :foresight :frustration :fury-cutter :hail :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :leer :mega-kick :mega-punch :mimic :mud-slap :protect :rage :rain-dance :rest :return :reversal :roar :rock-slide :rock-smash :rock-tomb :safeguard :sandstorm :secret-power :seismic-toss :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :stomp :strength :substitute :sunny-day :surf :swagger :tail-whip :thief :thunder :thunderbolt :thunderpunch :toxic :water-pulse :body-slam :icy-wind :mimic :seismic-toss :sing])

(deflearnset :dp
  :kangaskhan [:aerial-ace :aqua-tail :attract :avalanche :bite :blizzard :brick-break :captivate :comet-punch :counter :crunch :crush-claw :cut :dig :disable :dizzy-punch :double-edge :double-hit :double-team :drain-punch :earthquake :endeavor :endure :facade :fake-out :fire-blast :fire-punch :flamethrower :fling :focus-blast :focus-energy :focus-punch :foresight :frustration :fury-cutter :giga-impact :hail :hammer-arm :headbutt :helping-hand :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :leer :low-kick :mega-punch :mud-slap :natural-gift :outrage :protect :rage :rain-dance :rest :return :reversal :roar :rock-climb :rock-slide :rock-smash :rock-tomb :safeguard :sandstorm :secret-power :shadow-ball :shadow-claw :shock-wave :sleep-talk :snore :solarbeam :spite :stomp :strength :substitute :sucker-punch :sunny-day :surf :swagger :tail-whip :thief :thunder :thunderbolt :thunderpunch :toxic :uproar :water-pulse :whirlpool :body-slam :icy-wind :mimic :seismic-toss :sing])

(deflearnset :bw
  :kangaskhan [:aerial-ace :aqua-tail :attract :bite :blizzard :brick-break :bulldoze :chip-away :circle-throw :comet-punch :counter :covet :crunch :crush-claw :cut :dig :disable :dizzy-punch :double-edge :double-hit :double-team :drain-punch :earthquake :endeavor :endure :facade :fake-out :fire-blast :fire-punch :flamethrower :fling :focus-blast :focus-energy :focus-punch :foresight :frustration :giga-impact :hail :hammer-arm :helping-hand :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :incinerate :iron-tail :leer :low-kick :mega-punch :outrage :protect :rage :rain-dance :rest :retaliate :return :reversal :roar :rock-slide :rock-smash :rock-tomb :round :safeguard :sandstorm :shadow-ball :shadow-claw :sleep-talk :snore :solarbeam :spite :stomp :strength :substitute :sucker-punch :sunny-day :surf :swagger :tail-whip :thief :thunder :thunderbolt :thunderpunch :toxic :trump-card :uproar :work-up :body-slam :icy-wind :mimic :seismic-toss :sing])
