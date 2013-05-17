
(in-ns 'smogon.dex)

(defpokemon :zubat
  :name "Zubat"
  :introduced-in :rb
  :types [[:flying :poison]]
  :egggroups [[:flying]]
  :abilities [[:inner-focus]
              :bw [:infiltrator :inner-focus]]
  :stats [[40 45 35 40 55]
          :gs [40 45 35 30 40 55]]
  :weight [7.5]
  :height [0.8])

(defpokemon :golbat
  :name "Golbat"
  :introduced-in :rb
  :types [[:flying :poison]]
  :egggroups [[:flying]]
  :abilities [[:inner-focus]
              :bw [:infiltrator :inner-focus]]
  :stats [[75 80 70 75 90]
          :gs [75 80 70 65 75 90]]
  :weight [55]
  :height [1.6])

(defpokemon :crobat
  :name "Crobat"
  :introduced-in :gs
  :types [[:flying :poison]]
  :egggroups [[:flying]]
  :abilities [[:inner-focus]
              :bw [:infiltrator :inner-focus]]
  :stats [[85 90 80 70 80 130]]
  :weight [75]
  :height [1.8])

(deffamily (familychain :zubat :golbat :crobat))

(deflearnset :rb
  :zubat [:bide :bite :confuse-ray :double-edge :double-team :haze :leech-life :mega-drain :mimic :rage :razor-wind :rest :substitute :supersonic :swift :take-down :toxic :whirlwind :wing-attack :flail]
  :golbat [:hyper-beam :screech])

(deflearnset :gs
  :zubat [:attract :bite :confuse-ray :curse :detect :double-team :endure :faint-attack :frustration :giga-drain :gust :haze :hidden-power :leech-life :mean-look :protect :pursuit :quick-attack :rest :return :sleep-talk :snore :steel-wing :sunny-day :supersonic :swagger :swift :thief :toxic :whirlwind :wing-attack :flail]
  :golbat [:hyper-beam :screech]
  :crobat [:fly])

(deflearnset :rs
  :zubat [:aerial-ace :air-cutter :astonish :attract :bite :confuse-ray :curse :double-edge :double-team :endure :facade :faint-attack :frustration :giga-drain :gust :haze :hidden-power :leech-life :mean-look :mimic :poison-fang :protect :pursuit :quick-attack :rain-dance :rest :return :secret-power :shadow-ball :sleep-talk :sludge-bomb :snatch :snore :steel-wing :substitute :sunny-day :supersonic :swagger :swift :taunt :thief :torment :toxic :whirlwind :wing-attack :double-edge :mimic :mimic]
  :golbat [:hyper-beam :screech :double-edge :mimic]
  :crobat [:fly])

(deflearnset :dp
  :zubat [:aerial-ace :air-cutter :air-slash :astonish :attract :bite :brave-bird :captivate :confuse-ray :curse :defog :double-team :endure :facade :faint-attack :fly :frustration :giga-drain :gust :haze :heat-wave :hidden-power :hypnosis :leech-life :mean-look :nasty-plot :natural-gift :ominous-wind :payback :pluck :poison-fang :protect :pursuit :quick-attack :rain-dance :rest :return :roost :secret-power :shadow-ball :sleep-talk :sludge-bomb :snatch :snore :steel-wing :substitute :sunny-day :super-fang :supersonic :swagger :swift :tailwind :taunt :thief :torment :toxic :twister :u-turn :uproar :whirlwind :wing-attack :zen-headbutt :double-edge :mimic :mimic]
  :golbat [:giga-impact :hyper-beam :screech :double-edge :mimic]
  :crobat [:cross-poison :dark-pulse :sky-attack :x-scissor])

(deflearnset :bw
  :zubat [:acrobatics :aerial-ace :air-cutter :air-slash :astonish :attract :bite :brave-bird :confuse-ray :curse :defog :double-team :facade :faint-attack :fly :frustration :giga-drain :gust :haze :heat-wave :hidden-power :hypnosis :leech-life :mean-look :nasty-plot :payback :pluck :poison-fang :protect :pursuit :quick-attack :rain-dance :rest :return :roost :round :shadow-ball :sleep-talk :sludge-bomb :snatch :snore :steel-wing :substitute :sunny-day :super-fang :supersonic :swagger :swift :tailwind :taunt :thief :torment :toxic :u-turn :uproar :venoshock :whirlwind :wing-attack :zen-headbutt :double-edge :mimic :mimic]
  :golbat [:giga-impact :hyper-beam :screech :double-edge :mimic]
  :crobat [:cross-poison :dark-pulse :sky-attack :x-scissor])

