
(in-ns 'smogon.dex)


(defpokemon :ralts
  :name "Ralts"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:synchronize :trace]
              :bw [:synchronize :telepathy :trace]]
  :stats [[28 25 25 45 35 40]]
  :weight [6.6]
  :height [0.4])

(defpokemon :kirlia
  :name "Kirlia"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:synchronize :trace]
              :bw [:synchronize :telepathy :trace]]
  :stats [[38 35 35 65 55 50]]
  :weight [20.2]
  :height [0.8])

(defpokemon :gardevoir
  :name "Gardevoir"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:synchronize :trace]
              :bw [:synchronize :telepathy :trace]]
  :stats [[68 65 65 125 115 80]]
  :weight [48.4]
  :height [1.6])

(defpokemon :gallade
  :name "Gallade"
  :introduced-in :dp
  :types [[:fighting :psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:justified :steadfast]]
  :stats [[68 125 65 65 115 80]]
  :weight [52]
  :height [1.6])

(deffamily (familychain :ralts :kirlia [:gardevoir :gallade]))

(deflearnset :rs
  :ralts [:attract :body-slam :calm-mind :confusion :defense-curl :destiny-bond :disable :double-edge :double-team :dream-eater :endure :facade :fire-punch :flash :frustration :future-sight :growl :hidden-power :hypnosis :ice-punch :icy-wind :imprison :light-screen :mean-look :memento :mimic :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :reflect :rest :return :safeguard :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :taunt :teleport :thief :thunder-wave :thunderbolt :thunderpunch :torment :toxic :will-o-wisp :body-slam :double-edge :icy-wind :mimic :nightmare :shock-wave :sing]
  :kirlia [:magical-leaf :body-slam :double-edge :icy-wind :mimic :nightmare]
  :gardevoir [:hyper-beam :body-slam :double-edge :icy-wind :mimic :nightmare])

(deflearnset :dp
  :ralts [:attract :calm-mind :captivate :charge-beam :charm :confuse-ray :confusion :destiny-bond :disable :double-team :dream-eater :encore :endure :facade :fire-punch :flash :fling :frustration :future-sight :grass-knot :growl :grudge :headbutt :helping-hand :hidden-power :hypnosis :ice-punch :icy-wind :imprison :light-screen :lucky-chant :magic-coat :magical-leaf :mean-look :memento :mud-slap :natural-gift :pain-split :protect :psych-up :psychic :rain-dance :recycle :reflect :rest :return :safeguard :secret-power :shadow-ball :shadow-sneak :shock-wave :signal-beam :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :swift :taunt :teleport :thief :thunder-wave :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :will-o-wisp :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare :shock-wave :sing]
  :kirlia [ :body-slam :double-edge :icy-wind :mimic :nightmare]
  :gallade [:aerial-ace :brick-break :bulk-up :close-combat :cut :drain-punch :earthquake :false-swipe :feint :focus-blast :focus-punch :fury-cutter :giga-impact :hyper-beam :knock-off :leaf-blade :leer :low-kick :night-slash :poison-jab :psycho-cut :rock-slide :rock-smash :rock-tomb :slash :stone-edge :strength :swords-dance :vacuum-wave :x-scissor]
  :gardevoir [:energy-ball :heal-bell :healing-wish :wish :body-slam :double-edge :icy-wind :mimic :nightmare])

(deflearnset :bw
  :ralts [:ally-switch :attract :calm-mind :charge-beam :charm :confuse-ray :confusion :destiny-bond :disable :double-team :dream-eater :echoed-voice :encore :facade :fire-punch :flash :fling :frustration :future-sight :grass-knot :growl :grudge :heal-pulse :helping-hand :hidden-power :hyper-voice :hypnosis :ice-punch :icy-wind :imprison :light-screen :lucky-chant :magic-coat :magic-room :magical-leaf :mean-look :memento :pain-split :protect :psych-up :psychic :psyshock :rain-dance :recycle :reflect :rest :return :round :safeguard :shadow-ball :shadow-sneak :signal-beam :skill-swap :sleep-talk :snatch :snore :stored-power :substitute :sunny-day :swagger :synchronoise :taunt :telekinesis :teleport :thief :thunder-wave :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :will-o-wisp :wonder-room :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare :shock-wave :sing]
  :kirlia [ :body-slam :double-edge :icy-wind :mimic :nightmare]
  :gallade [:aerial-ace :brick-break :bulk-up :bulldoze :close-combat :cut :drain-punch :dual-chop :earthquake :false-swipe :feint :focus-blast :fury-cutter :giga-impact :hyper-beam :knock-off :leaf-blade :leer :low-kick :low-sweep :night-slash :poison-jab :psycho-cut :retaliate :rock-slide :rock-smash :rock-tomb :slash :stone-edge :strength :swords-dance :work-up :x-scissor]
  :gardevoir [:captivate :energy-ball :heal-bell :healing-wish :wish :body-slam :double-edge :icy-wind :mimic :nightmare])
