
(in-ns 'smogon.dex)

(defpokemon :deoxys
  :name "Deoxys"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure]]
  :stats [[50 150 50 150 50 150]]
  :weight [60.8]
  :height [1.7])

(defpokemon :deoxys-a
  :name "Deoxys-Attack"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure]]
  :stats [[50 180 20 180 20 150]]
  :weight [60.8]
  :height [1.7])

(defpokemon :deoxys-d
  :name "Deoxys-Defense"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure]]
  :stats [[50 70 160 70 160 90]]
  :weight [60.8]
  :height [1.7])

(defpokemon :deoxys-s
  :name "Deoxys-Speed"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure]]
  :stats [[50 95 90 95 90 180]]
  :weight [60.8]
  :height [1.7])

(deffamilygraph
  (familyclique :deoxys :deoxys-a :deoxys-d :deoxys-s))

(deflearnset :rs
  :deoxys [:aerial-ace :body-slam :brick-break :calm-mind :cosmic-power :cut :double-edge :double-team :dream-eater :facade :flash :focus-punch :frustration :hidden-power :hyper-beam :ice-beam :icy-wind :knock-off :leer :light-screen :mimic :night-shade :nightmare :protect :psychic :psycho-boost :pursuit :rain-dance :recover :reflect :rest :return :rock-smash :rock-tomb :safeguard :secret-power :seismic-toss :shadow-ball :shock-wave :skill-swap :snatch :solarbeam :strength :substitute :sunny-day :swagger :taunt :teleport :thunder :thunder-wave :thunderbolt :torment :toxic :water-pulse :wrap])

(deflearnset :dp
  :deoxys [:aerial-ace :avalanche :brick-break :calm-mind :charge-beam :cosmic-power :cut :double-team :drain-punch :dream-eater :endure :energy-ball :facade :fire-punch :flash :flash-cannon :fling :focus-blast :focus-punch :frustration :giga-impact :grass-knot :gravity :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :knock-off :leer :light-screen :low-kick :magic-coat :mud-slap :natural-gift :night-shade :poison-jab :protect :psych-up :psychic :psycho-boost :psycho-shift :pursuit :rain-dance :recover :recycle :reflect :rest :return :rock-slide :rock-smash :rock-tomb :role-play :safeguard :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snatch :snore :solarbeam :stealth-rock :strength :substitute :sunny-day :swagger :swift :taunt :teleport :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :water-pulse :wrap :zen-headbutt :detect :extremespeed :counter :mirror-coat :meteor-mash :superpower])

(deflearnset :bw
  :deoxys [:aerial-ace :ally-switch :bind :brick-break :calm-mind :charge-beam :cosmic-power :cut :dark-pulse :double-team :drain-punch :dream-eater :energy-ball :facade :fire-punch :flash :flash-cannon :fling :focus-blast :frustration :giga-impact :grass-knot :gravity :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :knock-off :leer :light-screen :low-kick :low-sweep :magic-coat :nasty-plot :night-shade :poison-jab :protect :psych-up :psychic :psycho-boost :psycho-shift :psyshock :pursuit :rain-dance :recover :recycle :reflect :rest :return :rock-slide :rock-smash :rock-tomb :role-play :round :safeguard :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :solarbeam :stealth-rock :strength :substitute :sunny-day :swagger :taunt :telekinesis :teleport :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :wonder-room :wrap :zen-headbutt :detect :extremespeed :counter :mirror-coat :meteor-mash :superpower])
