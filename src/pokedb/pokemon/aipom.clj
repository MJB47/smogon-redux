
(in-ns 'smogon.dex)

(defpokemon :aipom
  :name "Aipom"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:pickup :run-away]
              :bw [:pickup :run-away :skill-link]]
  :stats [[55 70 55 40 55 85]]
  :weight [11.5]
  :height [0.8])

(defpokemon :ambipom
  :name "Ambipom"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:pickup :skill-link :technician]]
  :stats [[75 100 66 60 66 115]]
  :weight [20.3]
  :height [1.2])

(deffamily (familychain :aipom :ambipom))

(deflearnset :gs
  :aipom [:agility :attract :baton-pass :beat-up :counter :curse :cut :defense-curl :detect :double-team :doubleslap :dream-eater :dynamicpunch :endure :fire-punch :frustration :fury-cutter :fury-swipes :headbutt :hidden-power :ice-punch :iron-tail :mud-slap :nightmare :protect :pursuit :rest :return :rock-smash :sand-attack :scratch :screech :shadow-ball :slam :sleep-talk :snore :spite :strength :sunny-day :swagger :swift :tail-whip :thief :thunder :thunderbolt :thunderpunch :toxic :zap-cannon])

(deflearnset :rs
  :aipom [:aerial-ace :agility :astonish :attract :baton-pass :beat-up :body-slam :brick-break :counter :cut :defense-curl :dig :double-edge :double-team :doubleslap :dream-eater :dynamicpunch :endure :facade :fire-punch :focus-punch :frustration :fury-cutter :fury-swipes :hidden-power :ice-punch :iron-tail :mega-kick :mega-punch :metronome :mimic :mud-slap :nightmare :protect :pursuit :rain-dance :rest :return :rock-smash :sand-attack :scratch :screech :secret-power :seismic-toss :shadow-ball :shock-wave :slam :sleep-talk :snatch :snore :solarbeam :spite :strength :substitute :sunny-day :swagger :swift :tail-whip :taunt :thief :thunder :thunder-wave :thunderbolt :thunderpunch :tickle :toxic :water-pulse])

(deflearnset :dp
  :aipom [:aerial-ace :agility :astonish :attract :baton-pass :beat-up :bounce :brick-break :captivate :counter :covet :cut :dig :double-hit :double-team :doubleslap :dream-eater :endure :facade :fake-out :fire-punch :fling :focus-punch :frustration :fury-cutter :fury-swipes :grass-knot :gunk-shot :headbutt :hidden-power :ice-punch :iron-tail :knock-off :last-resort :low-kick :mud-slap :nasty-plot :natural-gift :payback :protect :pursuit :rain-dance :rest :return :rock-smash :role-play :sand-attack :scratch :screech :secret-power :seed-bomb :shadow-ball :shadow-claw :shock-wave :slam :sleep-talk :snatch :snore :solarbeam :spite :strength :substitute :sunny-day :swagger :swift :tail-whip :taunt :thief :thunder :thunder-wave :thunderbolt :thunderpunch :tickle :toxic :u-turn :uproar :water-pulse]
  :ambipom [:giga-impact :hyper-beam])

(deflearnset :bw
  :aipom [:acrobatics :aerial-ace :agility :astonish :attract :baton-pass :beat-up :body-slam :bounce :brick-break :counter :covet :cut :dig :double-edge :double-hit :double-team :doubleslap :dream-eater :facade :fake-out :fire-punch :fling :foul-play :frustration :fury-swipes :grass-knot :gunk-shot :hidden-power :hone-claws :ice-punch :iron-tail :knock-off :last-resort :low-kick :low-sweep :mimic :nasty-plot :nightmare :payback :protect :pursuit :rain-dance :rest :retaliate :return :revenge :rock-smash :role-play :round :sand-attack :scratch :screech :seed-bomb :seismic-toss :shadow-ball :shadow-claw :shock-wave :slam :sleep-talk :snatch :snore :solarbeam :spite :strength :substitute :sunny-day :swagger :swift :switcheroo :tail-whip :taunt :thief :thunder :thunder-wave :thunderbolt :thunderpunch :tickle :toxic :u-turn :uproar :work-up]
  :ambipom [:giga-impact :hyper-beam])
