
(in-ns 'smogon.dex)


(defpokemon :ekans
  :name "Ekans"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[:dragon :ground]]
  :abilities [[:intimidate :shed-skin]
              :bw [:intimidate :shed-skin :unnerve]]
  :stats [[35 60 44 40 55]
          :gs [35 60 44 40 54 55]]
  :weight [6.9]
  :height [2])

(defpokemon :arbok
  :name "Arbok"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[:dragon :ground]]
  :abilities [[:intimidate :shed-skin]
              :bw [:intimidate :shed-skin :unnerve]]
  :stats [[60 85 69 65 80]
          :gs [60 85 69 65 79 80]]
  :weight [65]
  :height [3.5])

(deffamily (familychain :ekans :arbok))

(deflearnset :rb
  :ekans [:acid :bide :bite :body-slam :dig :double-edge :double-team :earthquake :fissure :glare :leer :mega-drain :mimic :poison-sting :rage :rest :rock-slide :screech :skull-bash :strength :substitute :take-down :toxic :wrap]
  :arbok [:hyper-beam])

(deflearnset :gs
  :ekans [:acid :attract :beat-up :bite :crunch :curse :dig :double-team :earthquake :endure :frustration :giga-drain :glare :haze :headbutt :hidden-power :leer :poison-sting :protect :pursuit :rest :return :screech :slam :sleep-talk :sludge-bomb :snore :spite :strength :sunny-day :swagger :thief :toxic :wrap]
  :arbok [:hyper-beam])

(deflearnset :rs
  :ekans [:acid :attract :beat-up :bite :body-slam :dig :double-edge :double-team :earthquake :endure :facade :frustration :giga-drain :glare :haze :hidden-power :iron-tail :leer :mimic :poison-fang :poison-sting :protect :pursuit :rain-dance :rest :return :rock-slide :screech :secret-power :slam :sleep-talk :sludge-bomb :snatch :snore :spit-up :spite :stockpile :strength :substitute :sunny-day :swagger :swallow :thief :torment :toxic :wrap :body-slam :double-edge :mimic]
  :arbok [:hyper-beam])

(deflearnset :dp
  :ekans [:acid :aqua-tail :attract :beat-up :bite :captivate :dark-pulse :dig :disable :double-team :earthquake :endure :facade :frustration :gastro-acid :giga-drain :glare :gunk-shot :haze :headbutt :hidden-power :iron-tail :leer :mud-bomb :natural-gift :payback :poison-fang :poison-jab :poison-sting :poison-tail :protect :pursuit :rain-dance :rest :return :rock-slide :rock-tomb :scary-face :screech :secret-power :seed-bomb :slam :sleep-talk :sludge-bomb :snatch :snore :spit-up :spite :stockpile :strength :substitute :sunny-day :swagger :swallow :switcheroo :thief :torment :toxic :wrap :body-slam :double-edge :mimic]
  :arbok [:crunch :fire-fang :giga-impact :hyper-beam :ice-fang :thunder-fang])

(deflearnset :bw
  :ekans [:acid :acid-spray :aqua-tail :attract :beat-up :bind :bite :bulldoze :coil :dark-pulse :dig :disable :double-team :earthquake :facade :frustration :gastro-acid :giga-drain :glare :gunk-shot :haze :hidden-power :iron-tail :leer :mud-bomb :payback :poison-fang :poison-jab :poison-sting :poison-tail :protect :pursuit :rain-dance :rest :return :rock-slide :rock-tomb :round :scary-face :screech :seed-bomb :slam :sleep-talk :sludge-bomb :sludge-wave :snatch :snore :spit-up :spite :stockpile :strength :substitute :sucker-punch :sunny-day :swagger :swallow :switcheroo :thief :torment :toxic :venoshock :wrap :body-slam :double-edge :mimic]
  :arbok [:crunch :dragon-tail :fire-fang :giga-impact :hyper-beam :ice-fang :thunder-fang])
