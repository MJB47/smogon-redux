
(in-ns 'smogon.dex)


(defpokemon :zangoose
  :name "Zangoose"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:immunity]
              :bw [:immunity :toxic-boost]]
  :stats [[73 115 60 60 60 90]]
  :weight [40.3]
  :height [1.3])

(deffamily (familychain :zangoose))

(deflearnset :rs
  :zangoose [:aerial-ace :attract :blizzard :body-slam :brick-break :counter :crush-claw :curse :defense-curl :detect :dig :double-edge :double-kick :double-team :dynamicpunch :endure :facade :false-swipe :fire-blast :fire-punch :flail :flamethrower :focus-punch :frustration :fury-cutter :giga-drain :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :leer :mega-kick :mega-punch :mimic :mud-slap :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :roar :rock-slide :rock-smash :rollout :scratch :secret-power :seismic-toss :shadow-ball :shock-wave :slash :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :swift :swords-dance :taunt :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :water-pulse :body-slam :double-edge :icy-wind :mimic :refresh :seismic-toss :thunder-wave])

(deflearnset :dp
  :zangoose [:aerial-ace :attract :blizzard :brick-break :captivate :close-combat :counter :crush-claw :curse :detect :dig :disable :double-hit :double-kick :double-team :embargo :endeavor :endure :facade :false-swipe :fire-blast :fire-punch :flail :flamethrower :fling :focus-blast :focus-punch :frustration :fury-cutter :fury-swipes :giga-drain :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :knock-off :last-resort :leer :low-kick :metal-claw :mud-slap :natural-gift :night-slash :payback :poison-jab :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :roar :rock-climb :rock-slide :rock-smash :rock-tomb :rollout :scratch :secret-power :shadow-ball :shadow-claw :shock-wave :slash :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :swift :swords-dance :taunt :thief :thunder :thunderbolt :thunderpunch :toxic :water-pulse :x-scissor :body-slam :double-edge :icy-wind :mimic :refresh :seismic-toss :thunder-wave])

(deflearnset :bw
  :zangoose [:aerial-ace :attract :blizzard :brick-break :close-combat :counter :crush-claw :curse :detect :dig :disable :double-hit :double-kick :double-team :embargo :endeavor :facade :false-swipe :feint :final-gambit :fire-blast :fire-punch :flail :flamethrower :fling :focus-blast :frustration :fury-cutter :fury-swipes :giga-drain :hidden-power :hone-claws :ice-beam :ice-punch :icy-wind :incinerate :iron-tail :knock-off :last-resort :leer :low-kick :metal-claw :night-slash :payback :poison-jab :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :retaliate :return :revenge :roar :rock-slide :rock-smash :rock-tomb :round :scratch :shadow-ball :shadow-claw :slash :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :swords-dance :taunt :thief :thunder :thunderbolt :thunderpunch :toxic :work-up :x-scissor :body-slam :double-edge :icy-wind :mimic :refresh :seismic-toss :thunder-wave])
