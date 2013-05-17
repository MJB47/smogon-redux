
(in-ns 'smogon.dex)


(defpokemon :houndour
  :name "Houndour"
  :introduced-in :gs
  :types [[:dark :fire]]
  :egggroups [[:ground]]
  :abilities [[:early-bird :flash-fire]
              :bw [:early-bird :flash-fire :unnerve]]
  :stats [[45 60 30 80 50 65]]
  :weight [10.8]
  :height [0.6])

(defpokemon :houndoom
  :name "Houndoom"
  :introduced-in :gs
  :types [[:dark :fire]]
  :egggroups [[:ground]]
  :abilities [[:early-bird :flash-fire]
              :bw [:early-bird :flash-fire :unnerve]]
  :stats [[75 90 50 110 80 95]]
  :weight [35]
  :height [1.4])

(deffamily (familychain :houndour :houndoom))

(deflearnset :gs
  :houndour [:attract :beat-up :bite :counter :crunch :curse :detect :double-team :dream-eater :ember :endure :faint-attack :fire-blast :fire-spin :flamethrower :frustration :headbutt :hidden-power :iron-tail :leer :mud-slap :nightmare :protect :pursuit :rage :rest :return :reversal :roar :rock-smash :shadow-ball :sleep-talk :sludge-bomb :smog :snore :solarbeam :spite :sunny-day :swagger :swift :thief :toxic]
  :houndoom [:hyper-beam :strength])

(deflearnset :rs
  :houndour [:attract :beat-up :bite :body-slam :counter :crunch :double-edge :double-team :dream-eater :ember :endure :facade :faint-attack :fire-blast :fire-spin :flamethrower :frustration :hidden-power :howl :iron-tail :leer :mimic :mud-slap :nightmare :odor-sleuth :overheat :protect :pursuit :rage :rest :return :reversal :roar :rock-smash :secret-power :shadow-ball :sleep-talk :sludge-bomb :smog :snatch :snore :solarbeam :spite :substitute :sunny-day :swagger :swift :taunt :thief :torment :toxic :will-o-wisp :body-slam :charm :double-edge :mimic :nightmare]
  :houndoom [:hyper-beam :strength :body-slam :double-edge :mimic])

(deflearnset :dp
  :houndour [:attract :beat-up :bite :captivate :counter :crunch :dark-pulse :double-team :dream-eater :embargo :ember :endure :facade :faint-attack :feint :fire-blast :fire-fang :fire-spin :flamethrower :frustration :headbutt :heat-wave :hidden-power :howl :iron-tail :leer :mud-slap :nasty-plot :natural-gift :odor-sleuth :overheat :payback :protect :punishment :pursuit :rage :rest :return :reversal :roar :rock-smash :role-play :secret-power :shadow-ball :sleep-talk :sludge-bomb :smog :snatch :snore :solarbeam :spite :substitute :sucker-punch :sunny-day :super-fang :swagger :swift :taunt :thief :thunder-fang :torment :toxic :uproar :will-o-wisp :body-slam :charm :double-edge :mimic :nightmare]
  :houndoom [:giga-impact :hyper-beam :strength :body-slam :double-edge :mimic])

(deflearnset :bw
  :houndour [:attract :beat-up :bite :counter :crunch :dark-pulse :double-team :dream-eater :embargo :ember :facade :faint-attack :feint :fire-blast :fire-fang :fire-spin :flame-charge :flamethrower :foul-play :frustration :heat-wave :hidden-power :howl :hyper-voice :incinerate :inferno :iron-tail :leer :nasty-plot :odor-sleuth :overheat :payback :protect :punishment :pursuit :rage :rest :retaliate :return :reversal :roar :rock-smash :role-play :round :shadow-ball :sleep-talk :sludge-bomb :smog :snarl :snatch :snore :solarbeam :spite :substitute :sucker-punch :sunny-day :super-fang :swagger :taunt :thief :thunder-fang :torment :toxic :uproar :will-o-wisp :body-slam :charm :double-edge :mimic :nightmare]
  :houndoom [:giga-impact :hyper-beam :strength :body-slam :double-edge :mimic])
