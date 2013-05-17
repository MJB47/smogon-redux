
(in-ns 'smogon.dex)

(defpokemon :nidoran-f
  :name "NidoranF"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[:ground :monster]]
  :abilities [[:poison-point]
              :dp [:poison-point :rivalry]
              :bw [:hustle :poison-point :rivalry]]
  :stats [[55 47 52 40 41]
          :gs [55 47 52 40 40 41]]
  :weight [7]
  :height [0.4])

(defpokemon :nidorina
  :name "Nidorina"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[]]
  :abilities [[:poison-point]
              :dp [:poison-point :rivalry]
              :bw [:hustle :poison-point :rivalry]]
  :stats [[70 62 67 55 56]
          :gs [70 62 67 55 55 56]]
  :weight [20]
  :height [0.8])

(defpokemon :nidoqueen
  :name "Nidoqueen"
  :introduced-in :rb
  :types [[:ground :poison]]
  :egggroups [[]]
  :abilities [[:poison-point]
              :dp [:poison-point :rivalry]
              :bw [:poison-point :rivalry :sheer-force]]
  :stats [[90 82 87 75 76]
          :gs [90 82 87 75 85 76]]
  :weight [60]
  :height [1.3])

(deffamily (familychain :nidoran-f :nidorina :nidoqueen))

(deflearnset :rb
  :nidoran-f [:bide :bite :blizzard :body-slam :double-edge :double-kick :double-team :fury-swipes :growl :mimic :poison-sting :rage :reflect :rest :scratch :skull-bash :substitute :tackle :tail-whip :take-down :thunder :thunderbolt :toxic :lovely-kiss :sweet-kiss :moonlight]
  :nidorina [:bubblebeam :horn-drill :ice-beam :water-gun]
  :nidoqueen [:counter :earthquake :fire-blast :fissure :hyper-beam :mega-kick :mega-punch :pay-day :rock-slide :seismic-toss :strength :submission :surf])

(deflearnset :gs
  :nidoran-f [:attract :beat-up :bite :blizzard :charm :counter :curse :defense-curl :detect :disable :double-kick :double-team :endure :focus-energy :frustration :fury-swipes :growl :headbutt :hidden-power :iron-tail :mud-slap :poison-sting :protect :rain-dance :rest :return :scratch :sleep-talk :snore :sunny-day :supersonic :swagger :tackle :tail-whip :take-down :thief :thunder :thunderbolt :toxic :lovely-kiss :sweet-kiss :moonlight]
  :nidorina [:ice-beam :rock-smash :strength]
  :nidoqueen [:body-slam :dynamicpunch :earthquake :fire-blast :fire-punch :flamethrower :fury-cutter :hyper-beam :ice-punch :icy-wind :roar :sandstorm :shadow-ball :surf :thunderpunch])

(deflearnset :rs
  :nidoran-f [:aerial-ace :attract :beat-up :bite :blizzard :body-slam :charm :counter :crunch :cut :defense-curl :dig :disable :double-edge :double-kick :double-team :endure :facade :flatter :focus-energy :frustration :fury-swipes :growl :helping-hand :hidden-power :ice-beam :iron-tail :mimic :mud-slap :poison-sting :protect :rain-dance :rest :return :rock-smash :scratch :secret-power :shock-wave :sleep-talk :sludge-bomb :snore :strength :substitute :sunny-day :supersonic :swagger :tail-whip :take-down :thief :thunder :thunderbolt :toxic :water-pulse :body-slam :double-edge :mimic]
  :nidorina [ :body-slam :double-edge :mimic]
  :nidoqueen [:brick-break :dynamicpunch :earthquake :fire-blast :fire-punch :flamethrower :focus-punch :fury-cutter :hyper-beam :ice-punch :icy-wind :mega-kick :mega-punch :roar :rock-slide :rock-tomb :sandstorm :seismic-toss :shadow-ball :superpower :surf :taunt :thunderpunch :torment :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :nidoran-f [:aerial-ace :attract :beat-up :bite :blizzard :captivate :charm :counter :crunch :cut :dig :disable :double-kick :double-team :endure :facade :flatter :focus-energy :frustration :fury-swipes :growl :headbutt :helping-hand :hidden-power :ice-beam :iron-tail :mud-slap :natural-gift :poison-fang :poison-jab :poison-sting :protect :pursuit :rain-dance :rest :return :rock-smash :scratch :secret-power :shadow-claw :shock-wave :skull-bash :sleep-talk :sludge-bomb :snore :strength :substitute :sunny-day :super-fang :supersonic :swagger :tail-whip :take-down :thief :thunder :thunderbolt :toxic :toxic-spikes :water-pulse :body-slam :double-edge :mimic]
  :nidorina [ :body-slam :double-edge :mimic]
  :nidoqueen [:aqua-tail :avalanche :body-slam :brick-break :dragon-pulse :earth-power :earthquake :fire-blast :fire-punch :flamethrower :fling :focus-blast :focus-punch :fury-cutter :giga-impact :hyper-beam :ice-punch :icy-wind :outrage :roar :rock-climb :rock-slide :rock-tomb :sandstorm :shadow-ball :stealth-rock :stone-edge :superpower :surf :taunt :thunderpunch :torment :uproar :whirlpool :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :nidoran-f [:aerial-ace :attract :beat-up :bite :blizzard :captivate :charm :chip-away :counter :crunch :cut :dig :disable :double-kick :double-team :endure :facade :flatter :focus-energy :frustration :fury-swipes :growl :helping-hand :hidden-power :hone-claws :ice-beam :iron-tail :poison-fang :poison-jab :poison-sting :poison-tail :protect :pursuit :rain-dance :rest :return :rock-smash :round :scratch :shadow-claw :skull-bash :sleep-talk :sludge-bomb :snore :strength :substitute :sunny-day :super-fang :supersonic :swagger :tail-whip :take-down :thief :thunder :thunderbolt :toxic :toxic-spikes :venoshock :sucker-punch :body-slam :double-edge :mimic]
  :nidorina [:echoed-voice :body-slam :body-slam :body-slam :body-slam :double-edge :mimic]
  :nidoqueen [:aqua-tail :body-slam :brick-break :bulldoze :dragon-pulse :dragon-tail :earth-power :earthquake :fire-blast :fire-punch :flamethrower :fling :focus-blast :giga-impact :hyper-beam :ice-punch :icy-wind :incinerate :outrage :quash :roar :rock-slide :rock-tomb :sandstorm :shadow-ball :sludge-wave :smack-down :stealth-rock :stone-edge :superpower :surf :taunt :thunderpunch :torment :uproar :double-edge :icy-wind :mimic :seismic-toss])
