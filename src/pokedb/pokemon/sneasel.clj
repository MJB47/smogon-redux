
(in-ns 'smogon.dex)


(defpokemon :sneasel
  :name "Sneasel"
  :introduced-in :gs
  :types [[:dark :ice]]
  :egggroups [[:ground]]
  :abilities [[:inner-focus :keen-eye]
              :bw [:inner-focus :keen-eye :pickpocket]]
  :stats [[55 95 55 35 75 115]]
  :weight [28]
  :height [0.9])

(defpokemon :weavile
  :name "Weavile"
  :introduced-in :dp
  :types [[:dark :ice]]
  :egggroups [[:ground]]
  :abilities [[:pickpocket :pressure]]
  :stats [[70 120 65 45 85 125]]
  :weight [34]
  :height [1.1])

(deffamily (familychain :sneasel :weavile))

(deflearnset :gs
  :sneasel [:agility :attract :beat-up :bite :blizzard :counter :curse :cut :defense-curl :detect :dig :double-team :dream-eater :dynamicpunch :endure :faint-attack :foresight :frustration :fury-cutter :fury-swipes :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :leer :metal-claw :mud-slap :nightmare :protect :psych-up :quick-attack :rain-dance :reflect :rest :return :rock-smash :scratch :screech :shadow-ball :slash :sleep-talk :snore :spite :strength :surf :swagger :swift :thief :toxic])

(deflearnset :rs
  :sneasel [:aerial-ace :agility :attract :beat-up :bite :blizzard :brick-break :calm-mind :counter :crush-claw :cut :defense-curl :dig :double-edge :double-team :dream-eater :dynamicpunch :endure :facade :faint-attack :fake-out :focus-punch :foresight :frustration :fury-cutter :fury-swipes :hail :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :leer :metal-claw :mimic :mud-slap :nightmare :protect :psych-up :quick-attack :rain-dance :reflect :rest :return :rock-smash :scratch :screech :secret-power :shadow-ball :slash :sleep-talk :snatch :snore :spite :strength :substitute :sunny-day :surf :swagger :swift :swords-dance :taunt :thief :torment :toxic :double-edge :mimic :nightmare])

(deflearnset :dp
  :sneasel [:aerial-ace :agility :assist :attract :avalanche :beat-up :bite :blizzard :brick-break :calm-mind :captivate :counter :crush-claw :cut :dark-pulse :dig :double-hit :double-team :dream-eater :embargo :endure :facade :faint-attack :fake-out :false-swipe :fling :focus-punch :foresight :frustration :fury-cutter :fury-swipes :hail :headbutt :hidden-power :ice-beam :ice-punch :ice-shard :icy-wind :iron-tail :knock-off :leer :low-kick :metal-claw :mud-slap :natural-gift :payback :poison-jab :protect :psych-up :punishment :pursuit :quick-attack :rain-dance :reflect :rest :return :rock-smash :scratch :screech :secret-power :shadow-ball :shadow-claw :slash :sleep-talk :snatch :snore :spite :strength :substitute :sunny-day :surf :swagger :swift :swords-dance :taunt :thief :torment :toxic :whirlpool :x-scissor :double-edge :mimic :nightmare]
  :weavile [:assurance :focus-blast :giga-impact :hyper-beam :nasty-plot :night-slash :revenge])

(deflearnset :bw
  :sneasel [:aerial-ace :agility :assist :attract :avalanche :beat-up :bite :blizzard :brick-break :calm-mind :counter :crush-claw :cut :dark-pulse :dig :double-hit :double-team :dream-eater :embargo :facade :faint-attack :fake-out :false-swipe :feint :fling :foresight :foul-play :frustration :fury-swipes :hail :hidden-power :hone-claws :ice-beam :ice-punch :ice-shard :icy-wind :iron-tail :knock-off :leer :low-kick :low-sweep :metal-claw :payback :poison-jab :protect :psych-up :punishment :pursuit :quick-attack :rain-dance :reflect :rest :retaliate :return :rock-smash :round :scratch :screech :shadow-ball :shadow-claw :slash :sleep-talk :snarl :snatch :snore :spite :strength :substitute :sunny-day :surf :swagger :swords-dance :taunt :thief :torment :toxic :x-scissor :double-edge :mimic :nightmare]
  :weavile [:assurance :focus-blast :giga-impact :hyper-beam :nasty-plot :night-slash :revenge])
