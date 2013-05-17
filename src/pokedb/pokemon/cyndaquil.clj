
(in-ns 'smogon.dex)


(defpokemon :cyndaquil
  :name "Cyndaquil"
  :introduced-in :gs
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze]
              :bw [:blaze :flash-fire]]
  :stats [[39 52 43 60 50 65]]
  :weight [7.9]
  :height [0.5])

(defpokemon :quilava
  :name "Quilava"
  :introduced-in :gs
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze]
              :bw [:blaze :flash-fire]]
  :stats [[58 64 58 80 65 80]]
  :weight [19]
  :height [0.9])

(defpokemon :typhlosion
  :name "Typhlosion"
  :introduced-in :gs
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze]
              :bw [:blaze :flash-fire]]
  :stats [[78 84 78 109 85 100]]
  :weight [79.5]
  :height [1.7])

(deffamily (familychain :cyndaquil :quilava :typhlosion))

(deflearnset :gs
  :cyndaquil [:attract :curse :cut :defense-curl :detect :dig :double-team :ember :endure :fire-blast :flame-wheel :flamethrower :foresight :frustration :fury-swipes :headbutt :hidden-power :iron-tail :leer :mud-slap :protect :quick-attack :rest :return :reversal :rollout :sleep-talk :smokescreen :snore :submission :sunny-day :swagger :swift :tackle :thrash :toxic]
  :quilava [:fury-cutter :roar :rock-smash :strength]
  :typhlosion [:dynamicpunch :earthquake :fire-punch :hyper-beam :thunderpunch])

(deflearnset :rs
  :cyndaquil [:aerial-ace :attract :body-slam :covet :crush-claw :cut :defense-curl :dig :double-edge :double-team :ember :endure :facade :fire-blast :flame-wheel :flamethrower :foresight :frustration :fury-swipes :hidden-power :howl :leer :mimic :mud-slap :overheat :protect :quick-attack :rest :return :reversal :rollout :secret-power :sleep-talk :smokescreen :snore :substitute :sunny-day :swagger :swift :tackle :thrash :toxic]
  :quilava [:brick-break :focus-punch :fury-cutter :roar :rock-smash :strength]
  :typhlosion [:counter :dynamicpunch :earthquake :fire-punch :hyper-beam :mega-kick :mega-punch :rock-slide :seismic-toss :thunderpunch])

(deflearnset :dp
  :cyndaquil [:aerial-ace :attract :captivate :covet :crush-claw :cut :defense-curl :dig :double-edge :double-kick :double-team :ember :endure :eruption :extrasensory :facade :fire-blast :flame-wheel :flamethrower :flare-blitz :foresight :frustration :fury-swipes :headbutt :heat-wave :hidden-power :howl :lava-plume :leer :mud-slap :natural-gift :overheat :protect :quick-attack :rest :return :reversal :rollout :secret-power :sleep-talk :smokescreen :snore :substitute :sunny-day :swagger :swift :tackle :thrash :toxic :will-o-wisp]
  :quilava [:brick-break :focus-punch :fury-cutter :roar :rock-smash :strength]
  :typhlosion [:blast-burn :earthquake :fire-punch :fling :focus-blast :giga-impact :gyro-ball :hyper-beam :low-kick :rock-climb :rock-slide :rock-tomb :shadow-claw :solarbeam :thunderpunch])

(deflearnset :bw
  :cyndaquil [:aerial-ace :attract :covet :crush-claw :cut :defense-curl :dig :double-edge :double-kick :double-team :ember :eruption :extrasensory :facade :fire-blast :fire-pledge :flame-burst :flame-charge :flame-wheel :flamethrower :flare-blitz :foresight :frustration :fury-swipes :heat-wave :hidden-power :howl :incinerate :inferno :lava-plume :leer :nature-power :overheat :protect :quick-attack :rest :return :reversal :rollout :round :sleep-talk :smokescreen :snore :substitute :sunny-day :swagger :swift :tackle :thrash :toxic :wild-charge :will-o-wisp]
  :quilava [:brick-break :roar :rock-smash :strength]
  :typhlosion [:blast-burn :bulldoze :earthquake :fire-punch :fling :focus-blast :giga-impact :gyro-ball :hyper-beam :low-kick :rock-slide :rock-tomb :shadow-claw :solarbeam :thunderpunch])
