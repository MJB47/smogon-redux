
(in-ns 'smogon.dex)


(defpokemon :magby
  :name "Magby"
  :introduced-in :gs
  :types [[:fire]]
  :egggroups [[]]
  :abilities [[:flame-body]
              :bw [:flame-body :vital-spirit]]
  :stats [[45 75 37 70 55 83]]
  :weight [21.4]
  :height [0.7])

(defpokemon :magmar
  :name "Magmar"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:humanshape]]
  :abilities [[:flame-body]
              :bw [:flame-body :vital-spirit]]
  :stats [[65 95 57 85 93]
          :gs [65 95 57 100 85 93]]
  :weight [44.5]
  :height [1.3])

(defpokemon :magmortar
  :name "Magmortar"
  :introduced-in :dp
  :types [[:fire]]
  :egggroups [[:humanshape]]
  :abilities [[:flame-body :vital-spirit]]
  :stats [[75 95 67 125 95 83]]
  :weight [68]
  :height [1.6])

(deffamily (familychain :magby :magmar :magmortar))

(deflearnset :rb
  :magmar [:bide :body-slam :confuse-ray :counter :double-edge :double-team :ember :fire-blast :fire-punch :flamethrower :hyper-beam :leer :mega-kick :mega-punch :metronome :mimic :psychic :psywave :rage :rest :seismic-toss :skull-bash :smog :smokescreen :strength :submission :substitute :take-down :teleport :toxic])

(deflearnset :gs
  :magby [:attract :barrier :confuse-ray :cross-chop :curse :detect :double-team :dynamicpunch :ember :endure :fire-blast :fire-punch :flamethrower :frustration :headbutt :hidden-power :iron-tail :karate-chop :leer :mega-punch :mud-slap :protect :psychic :rest :return :screech :sleep-talk :smog :smokescreen :snore :sunny-day :swagger :thief :thunderpunch :toxic]
  :magmar [:hyper-beam :rock-smash :strength])

(deflearnset :rs
  :magby [:attract :barrier :body-slam :brick-break :confuse-ray :counter :cross-chop :double-edge :double-team :dynamicpunch :ember :endure :facade :fire-blast :fire-punch :flamethrower :focus-punch :frustration :hidden-power :iron-tail :karate-chop :leer :mega-kick :mega-punch :mimic :mud-slap :protect :psychic :rest :return :rock-smash :screech :secret-power :seismic-toss :sleep-talk :smog :smokescreen :snore :substitute :sunny-day :swagger :thief :thunderpunch :toxic :body-slam :double-edge :mimic :seismic-toss]
  :magmar [:hyper-beam :strength :body-slam :double-edge :follow-me :mimic :seismic-toss])

(deflearnset :dp
  :magby [:attract :barrier :belly-drum :brick-break :captivate :confuse-ray :cross-chop :double-team :dynamicpunch :ember :endure :facade :faint-attack :fire-blast :fire-punch :fire-spin :flamethrower :flare-blitz :fling :focus-punch :frustration :headbutt :heat-wave :helping-hand :hidden-power :iron-tail :karate-chop :lava-plume :leer :mach-punch :mega-punch :mud-slap :natural-gift :overheat :protect :psychic :rest :return :rock-smash :screech :secret-power :sleep-talk :smog :smokescreen :snore :substitute :sunny-day :swagger :thief :thunderpunch :toxic :uproar :will-o-wisp :body-slam :double-edge :mimic :seismic-toss]
  :magmar [:focus-blast :giga-impact :hyper-beam :low-kick :rock-climb :strength :body-slam :double-edge :follow-me :mimic :seismic-toss]
  :magmortar [:earthquake :rock-slide :rock-tomb :solarbeam :taunt :thunderbolt :torment])

(deflearnset :bw
  :magby [:attract :barrier :belly-drum :brick-break :clear-smog :confuse-ray :covet :cross-chop :double-team :dual-chop :dynamicpunch :ember :facade :faint-attack :fire-blast :fire-punch :fire-spin :flame-burst :flame-charge :flamethrower :flare-blitz :fling :focus-energy :frustration :heat-wave :helping-hand :hidden-power :incinerate :iron-tail :karate-chop :lava-plume :leer :mach-punch :mega-punch :overheat :protect :psychic :rest :return :rock-smash :round :screech :sleep-talk :smog :smokescreen :snore :substitute :sunny-day :swagger :thief :thunderpunch :toxic :uproar :will-o-wisp :body-slam :double-edge :mimic :seismic-toss]
  :magmar [:focus-blast :giga-impact :hyper-beam :low-kick :low-sweep :strength :body-slam :double-edge :follow-me :mimic :seismic-toss]
  :magmortar [:bulldoze :earthquake :rock-slide :rock-tomb :solarbeam :taunt :thunderbolt :torment])
