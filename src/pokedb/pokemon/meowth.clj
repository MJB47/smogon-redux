
(in-ns 'smogon.dex)

(defpokemon :meowth
  :name "Meowth"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:pickup]
              :dp [:pickup :technician]
              :bw [:pickup :technician :unnerve]]
  :stats [[40 45 35 40 90]
          :gs [40 45 35 40 40 90]]
  :weight [4.2]
  :height [0.4])

(defpokemon :persian
  :name "Persian"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:limber]
              :dp [:limber :technician]
              :bw [:limber :technician :unnerve]]
  :stats [[65 70 60 65 115]
          :gs [65 70 60 65 65 115]]
  :weight [32]
  :height [1])

(deffamily (familychain :meowth :persian))

(deflearnset :rb
  :meowth [:bide :bite :body-slam :bubblebeam :double-edge :double-team :fury-swipes :growl :mimic :pay-day :rage :rest :scratch :screech :skull-bash :slash :substitute :swift :take-down :thunder :thunderbolt :toxic :water-gun]
  :persian [:hyper-beam])

(deflearnset :gs
  :meowth [:amnesia :attract :bite :charm :curse :defense-curl :detect :double-team :dream-eater :endure :faint-attack :frustration :fury-swipes :growl :headbutt :hidden-power :hypnosis :icy-wind :iron-tail :mud-slap :nightmare :pay-day :protect :psych-up :rest :return :scratch :screech :shadow-ball :slash :sleep-talk :snore :spite :sunny-day :swagger :swift :thief :thunder :thunderbolt :toxic :zap-cannon]
  :persian [:hyper-beam :roar])

(deflearnset :rs
  :meowth [:aerial-ace :amnesia :assist :attract :bite :body-slam :charm :cut :defense-curl :dig :double-edge :double-team :dream-eater :endure :facade :faint-attack :fake-out :flash :frustration :fury-swipes :growl :hidden-power :hypnosis :icy-wind :iron-tail :mimic :mud-slap :nightmare :pay-day :protect :psych-up :rain-dance :rest :return :scratch :screech :secret-power :shadow-ball :shock-wave :slash :sleep-talk :snatch :snore :spite :substitute :sunny-day :swagger :swift :taunt :thief :thunder :thunderbolt :torment :toxic :water-pulse :body-slam :double-edge :icy-wind :mimic :nightmare :sing]
  :persian [:hyper-beam :roar :body-slam :double-edge :icy-wind :mimic :nightmare])

(deflearnset :dp
  :meowth [:aerial-ace :amnesia :assist :assurance :attract :bite :captivate :charm :cut :dark-pulse :dig :double-team :dream-eater :endure :facade :faint-attack :fake-out :feint :flail :flash :frustration :fury-swipes :growl :gunk-shot :headbutt :hidden-power :hypnosis :icy-wind :iron-tail :knock-off :last-resort :mud-slap :nasty-plot :natural-gift :night-slash :odor-sleuth :pay-day :payback :protect :psych-up :punishment :rain-dance :rest :return :scratch :screech :secret-power :seed-bomb :shadow-ball :shadow-claw :shock-wave :slash :sleep-talk :snatch :snore :spite :substitute :sunny-day :swagger :swift :tail-whip :taunt :thief :thunder :thunderbolt :torment :toxic :u-turn :uproar :water-pulse :body-slam :double-edge :icy-wind :mimic :nightmare :sing]
  :persian [:embargo :giga-impact :hyper-beam :power-gem :roar :switcheroo :body-slam :double-edge :icy-wind :mimic :nightmare])

(deflearnset :bw
  :meowth [:aerial-ace :amnesia :assist :assurance :attract :bite :captivate :charm :covet :cut :dark-pulse :dig :double-team :dream-eater :echoed-voice :facade :faint-attack :fake-out :feint :flail :flash :foul-play :frustration :fury-swipes :growl :gunk-shot :hidden-power :hone-claws :hyper-voice :hypnosis :icy-wind :iron-tail :knock-off :last-resort :nasty-plot :night-slash :odor-sleuth :pay-day :payback :protect :psych-up :punishment :rain-dance :rest :retaliate :return :round :scratch :screech :seed-bomb :shadow-ball :shadow-claw :slash :sleep-talk :snatch :snore :spite :substitute :sunny-day :swagger :tail-whip :taunt :thief :thunder :thunderbolt :torment :toxic :u-turn :uproar :work-up :sing :body-slam :double-edge :icy-wind :mimic :nightmare :sing]
  :persian [:embargo :giga-impact :hyper-beam :power-gem :roar :swift :switcheroo :body-slam :double-edge :icy-wind :mimic :nightmare])
