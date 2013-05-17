
(in-ns 'smogon.dex)


(defpokemon :koffing
  :name "Koffing"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[40 65 95 60 35]
          :gs [40 65 95 60 45 35]]
  :weight [1]
  :height [0.6])

(defpokemon :weezing
  :name "Weezing"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[65 90 120 85 60]
          :gs [65 90 120 85 70 60]]
  :weight [9.5]
  :height [1.2])

(deffamily (familychain :koffing :weezing))

(deflearnset :rb
  :koffing [:bide :double-team :explosion :fire-blast :haze :mimic :rage :rest :selfdestruct :sludge :smog :smokescreen :substitute :tackle :thunder :thunderbolt :toxic]
  :weezing [:hyper-beam])

(deflearnset :gs
  :koffing [:attract :curse :destiny-bond :double-team :endure :explosion :fire-blast :flamethrower :frustration :haze :hidden-power :pain-split :poison-gas :protect :psybeam :psywave :rest :return :rollout :screech :selfdestruct :sleep-talk :sludge :sludge-bomb :smog :smokescreen :snore :sunny-day :swagger :tackle :thief :thunder :thunderbolt :toxic :zap-cannon]
  :weezing [:hyper-beam])

(deflearnset :rs
  :koffing [:attract :destiny-bond :double-team :endure :explosion :facade :fire-blast :flamethrower :flash :frustration :haze :hidden-power :memento :mimic :pain-split :poison-gas :protect :psybeam :psywave :rain-dance :rest :return :rollout :screech :secret-power :selfdestruct :shadow-ball :shock-wave :sleep-talk :sludge :sludge-bomb :smog :smokescreen :snore :substitute :sunny-day :swagger :tackle :taunt :thief :thunder :thunderbolt :torment :toxic :will-o-wisp :mimic]
  :weezing [:hyper-beam :mimic])

(deflearnset :dp
  :koffing [:assurance :attract :captivate :curse :dark-pulse :destiny-bond :double-team :endure :explosion :facade :fire-blast :flamethrower :flash :frustration :grudge :gyro-ball :haze :hidden-power :memento :natural-gift :pain-split :payback :poison-gas :protect :psybeam :psywave :rain-dance :rest :return :rollout :screech :secret-power :selfdestruct :shadow-ball :shock-wave :sleep-talk :sludge :sludge-bomb :smog :smokescreen :snore :spite :substitute :sunny-day :swagger :tackle :taunt :thief :thunder :thunderbolt :torment :toxic :uproar :will-o-wisp :mimic]
  :weezing [:double-hit :giga-impact :hyper-beam :mimic])

(deflearnset :bw
  :koffing [:assurance :attract :clear-smog :curse :dark-pulse :destiny-bond :double-team :explosion :facade :fire-blast :flamethrower :flash :frustration :grudge :gyro-ball :haze :hidden-power :incinerate :memento :pain-split :payback :poison-gas :protect :psybeam :psywave :rain-dance :rest :return :round :screech :selfdestruct :shadow-ball :sleep-talk :sludge :sludge-bomb :smog :smokescreen :snore :spit-up :spite :stockpile :substitute :sunny-day :swagger :swallow :tackle :taunt :thief :thunder :thunderbolt :torment :toxic :uproar :venoshock :will-o-wisp :mimic]
  :weezing [:double-hit :giga-impact :hyper-beam :mimic])
