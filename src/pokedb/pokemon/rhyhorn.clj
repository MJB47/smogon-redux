
(in-ns 'smogon.dex)


(defpokemon :rhyhorn
  :name "Rhyhorn"
  :introduced-in :rb
  :types [[:ground :rock]]
  :egggroups [[:ground :monster]]
  :abilities [[:lightningrod :rock-head]
              :bw [:lightningrod :reckless :rock-head]]
  :stats [[80 85 95 30 25]
          :gs [80 85 95 30 30 25]]
  :weight [115]
  :height [1])

(defpokemon :rhydon
  :name "Rhydon"
  :introduced-in :rb
  :types [[:ground :rock]]
  :egggroups [[:ground :monster]]
  :abilities [[:lightningrod :rock-head]
              :bw [:lightningrod :reckless :rock-head]]
  :stats [[105 130 120 45 40]
          :gs [105 130 120 45 45 40]]
  :weight [120]
  :height [1.9])

(defpokemon :rhyperior
  :name "Rhyperior"
  :introduced-in :dp
  :types [[:ground :rock]]
  :egggroups [[:ground :monster]]
  :abilities [[:lightningrod :reckless :solid-rock]]
  :stats [[115 140 130 55 55 40]]
  :weight [282.8]
  :height [2.4])

(deffamily (familychain :rhyhorn :rhydon :rhyperior))

(deflearnset :rb
  :rhyhorn [:bide :body-slam :dig :double-edge :double-team :earthquake :fire-blast :fissure :fury-attack :horn-attack :horn-drill :leer :mimic :rage :rest :rock-slide :skull-bash :stomp :strength :substitute :tail-whip :take-down :thunder :thunderbolt :toxic]
  :rhydon [:blizzard :bubblebeam :counter :hyper-beam :ice-beam :mega-kick :mega-punch :pay-day :seismic-toss :submission :surf :water-gun])

(deflearnset :gs
  :rhyhorn [:attract :blizzard :counter :crunch :curse :dig :double-team :earthquake :endure :fire-blast :flamethrower :frustration :fury-attack :headbutt :hidden-power :horn-attack :horn-drill :ice-beam :icy-wind :iron-tail :magnitude :mud-slap :protect :pursuit :rest :return :reversal :roar :rock-slide :rock-smash :rollout :sandstorm :scary-face :sleep-talk :snore :stomp :strength :sunny-day :swagger :tail-whip :take-down :thrash :thunder :thunderbolt :toxic :zap-cannon]
  :rhydon [:dynamicpunch :fire-punch :fury-cutter :hyper-beam :surf :thunderpunch])

(deflearnset :rs
  :rhyhorn [:attract :blizzard :body-slam :counter :crunch :crush-claw :curse :dig :double-edge :double-team :earthquake :endure :facade :fire-blast :flamethrower :frustration :fury-attack :hidden-power :horn-attack :horn-drill :ice-beam :icy-wind :iron-tail :magnitude :megahorn :mimic :mud-slap :protect :rain-dance :rest :return :reversal :roar :rock-blast :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :scary-face :secret-power :shock-wave :sleep-talk :snore :stomp :strength :substitute :sunny-day :swagger :swords-dance :tail-whip :take-down :thief :thunder :thunderbolt :toxic :body-slam :double-edge :icy-wind :mimic]
  :rhydon [:brick-break :cut :dynamicpunch :fire-punch :focus-punch :fury-cutter :hyper-beam :mega-kick :mega-punch :seismic-toss :surf :thunderpunch :body-slam :double-edge :helping-hand :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :rhyhorn [:ancientpower :aqua-tail :attract :blizzard :captivate :counter :crunch :crush-claw :curse :dig :double-team :dragon-pulse :dragon-rush :earth-power :earthquake :endeavor :endure :facade :fire-blast :fire-fang :flamethrower :frustration :fury-attack :headbutt :hidden-power :horn-attack :horn-drill :ice-beam :ice-fang :icy-wind :iron-tail :magnitude :megahorn :mud-slap :natural-gift :payback :poison-jab :protect :rain-dance :rest :return :reversal :roar :rock-blast :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :scary-face :secret-power :shock-wave :skull-bash :sleep-talk :snore :spite :stealth-rock :stomp :stone-edge :strength :substitute :sunny-day :superpower :swagger :swords-dance :tail-whip :take-down :thief :thunder :thunder-fang :thunderbolt :toxic :uproar :body-slam :double-edge :icy-wind :mimic]
  :rhydon [:avalanche :block :brick-break :cut :fire-punch :fling :focus-blast :focus-punch :fury-cutter :giga-impact :hammer-arm :hyper-beam :ice-punch :outrage :shadow-claw :surf :thunderpunch :whirlpool :body-slam :double-edge :helping-hand :icy-wind :mimic :seismic-toss]
  :rhyperior [:flash-cannon :iron-head :rock-wrecker])

(deflearnset :bw
  :rhyhorn [:aqua-tail :attract :blizzard :bulldoze :chip-away :counter :crunch :crush-claw :curse :dig :double-team :dragon-pulse :dragon-rush :drill-run :earth-power :earthquake :endeavor :facade :fire-blast :fire-fang :flamethrower :frustration :fury-attack :hidden-power :horn-attack :horn-drill :ice-beam :ice-fang :icy-wind :incinerate :iron-tail :magnitude :megahorn :payback :poison-jab :protect :rain-dance :rest :return :reversal :roar :rock-blast :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :round :sandstorm :scary-face :skull-bash :sleep-talk :snore :spite :stealth-rock :stomp :stone-edge :strength :substitute :sunny-day :superpower :swagger :swords-dance :tail-whip :take-down :thief :thunder :thunder-fang :thunderbolt :toxic :uproar :body-slam :double-edge :icy-wind :mimic]
  :rhydon [:block :brick-break :cut :dragon-tail :fire-punch :fling :focus-blast :giga-impact :hammer-arm :hyper-beam :ice-punch :outrage :shadow-claw :smack-down :surf :thunderpunch :body-slam :double-edge :helping-hand :icy-wind :mimic :seismic-toss]
  :rhyperior [:flash-cannon :iron-head :rock-wrecker])
