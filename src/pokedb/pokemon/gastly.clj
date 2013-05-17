
(in-ns 'smogon.dex)


(defpokemon :gastly
  :name "Gastly"
  :introduced-in :rb
  :types [[:ghost :poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[30 35 30 100 80]
          :gs [30 35 30 100 35 80]]
  :weight [0.1]
  :height [1.3])

(defpokemon :haunter
  :name "Haunter"
  :introduced-in :rb
  :types [[:ghost :poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[45 50 45 115 95]
          :gs [45 50 45 115 55 95]]
  :weight [0.1]
  :height [1.6])

(defpokemon :gengar
  :name "Gengar"
  :introduced-in :rb
  :types [[:ghost :poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[60 65 60 130 110]
          :gs [60 65 60 130 75 110]]
  :weight [40.5]
  :height [1.5])

(deffamily (familychain :gastly :haunter :gengar))

(deflearnset :rb
  :gastly [:bide :confuse-ray :double-team :dream-eater :explosion :hypnosis :lick :mega-drain :mimic :night-shade :psychic :psywave :rage :rest :selfdestruct :substitute :thunder :thunderbolt :toxic]
  :gengar [:body-slam :counter :double-edge :hyper-beam :mega-kick :mega-punch :metronome :seismic-toss :skull-bash :strength :submission :take-down])

(deflearnset :gs
  :gastly [:attract :confuse-ray :curse :destiny-bond :double-team :dream-eater :endure :frustration :giga-drain :haze :hidden-power :hypnosis :lick :mean-look :night-shade :nightmare :perish-song :protect :psych-up :psychic :psywave :rain-dance :rest :return :shadow-ball :sleep-talk :snore :spite :sunny-day :swagger :thief :thunder :thunderbolt :toxic :zap-cannon]
  :gengar [:dynamicpunch :fire-punch :headbutt :hyper-beam :ice-punch :rock-smash :strength :thunderpunch])

(deflearnset :rs
  :gastly [:astonish :attract :confuse-ray :curse :destiny-bond :double-team :dream-eater :endure :explosion :facade :frustration :giga-drain :grudge :haze :hidden-power :hypnosis :lick :mean-look :mimic :night-shade :nightmare :perish-song :protect :psych-up :psychic :psywave :rain-dance :rest :return :secret-power :selfdestruct :shadow-ball :skill-swap :sleep-talk :sludge-bomb :snatch :snore :spite :substitute :sunny-day :swagger :taunt :thief :thunderbolt :torment :toxic :will-o-wisp :double-edge :mimic :selfdestruct]
  :haunter [:shadow-punch :double-edge :mimic :selfdestruct]
  :gengar [:body-slam :brick-break :counter :double-edge :dynamicpunch :fire-punch :focus-punch :hyper-beam :ice-punch :mega-kick :mega-punch :metronome :rock-smash :seismic-toss :strength :thunder :thunderpunch :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :dp
  :gastly [:astonish :attract :captivate :confuse-ray :curse :dark-pulse :destiny-bond :disable :double-team :dream-eater :embargo :endure :energy-ball :explosion :facade :fire-punch :frustration :giga-drain :grudge :haze :hidden-power :hypnosis :ice-punch :icy-wind :knock-off :lick :mean-look :natural-gift :night-shade :nightmare :ominous-wind :pain-split :payback :perish-song :protect :psych-up :psychic :psywave :rain-dance :rest :return :secret-power :shadow-ball :skill-swap :sleep-talk :sludge-bomb :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :thief :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :uproar :will-o-wisp :double-edge :mimic :selfdestruct]
  :haunter [:fling :poison-jab :shadow-claw :shadow-punch :double-edge :mimic :selfdestruct]
  :gengar [:brick-break :drain-punch :focus-blast :focus-punch :giga-impact :headbutt :hyper-beam :rock-smash :role-play :strength :thunder :body-slam :double-edge :mimic :seismic-toss :selfdestruct])

(deflearnset :bw
  :gastly [:astonish :attract :clear-smog :confuse-ray :curse :dark-pulse :destiny-bond :disable :double-team :dream-eater :embargo :energy-ball :explosion :facade :fire-punch :foul-play :frustration :giga-drain :grudge :haze :hex :hidden-power :hypnosis :ice-punch :icy-wind :knock-off :lick :mean-look :night-shade :nightmare :pain-split :payback :perish-song :protect :psych-up :psychic :psywave :rain-dance :rest :return :round :scary-face :shadow-ball :skill-swap :sleep-talk :sludge-bomb :smog :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :telekinesis :thief :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :uproar :venoshock :will-o-wisp :wonder-room :double-edge :mimic :selfdestruct]
  :haunter [:fling :poison-jab :shadow-claw :shadow-punch :double-edge :mimic :selfdestruct]
  :gengar [:brick-break :drain-punch :focus-blast :giga-impact :hyper-beam :rock-smash :role-play :strength :thunder :body-slam :double-edge :mimic :seismic-toss :selfdestruct])
