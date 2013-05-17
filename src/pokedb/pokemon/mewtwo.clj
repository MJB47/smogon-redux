
(in-ns 'smogon.dex)

(defpokemon :mewtwo
  :name "Mewtwo"
  :introduced-in :rb
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure :unnerve]]
  :stats [[106 110 90 154 130]
          :gs [106 110 90 154 90 130]]
  :weight [122]
  :height [2])

(deffamily (familychain :mewtwo))

(deflearnset :rb
  :mewtwo [:amnesia :barrier :bide :blizzard :body-slam :bubblebeam :confusion :counter :disable :double-edge :double-team :fire-blast :flash :hyper-beam :ice-beam :mega-kick :mega-punch :metronome :mimic :mist :pay-day :psychic :psywave :rage :recover :reflect :rest :seismic-toss :selfdestruct :skull-bash :solarbeam :strength :submission :substitute :swift :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic :tri-attack :water-gun])

(deflearnset :gs
  :mewtwo [:amnesia :barrier :blizzard :confusion :curse :detect :disable :double-team :dream-eater :dynamicpunch :endure :fire-blast :fire-punch :flamethrower :flash :frustration :future-sight :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :mist :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :recover :rest :return :rock-smash :safeguard :shadow-ball :sleep-talk :snore :solarbeam :strength :sunny-day :swagger :swift :thunder :thunderbolt :thunderpunch :toxic :zap-cannon])

(deflearnset :rs
  :mewtwo [:aerial-ace :amnesia :barrier :blizzard :body-slam :brick-break :bulk-up :calm-mind :confusion :counter :disable :double-edge :double-team :dream-eater :dynamicpunch :earthquake :endure :facade :fire-blast :fire-punch :flamethrower :flash :focus-punch :frustration :future-sight :hail :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :light-screen :mega-kick :mega-punch :metronome :mimic :mist :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :recover :reflect :rest :return :rock-smash :rock-tomb :safeguard :sandstorm :secret-power :seismic-toss :selfdestruct :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :solarbeam :strength :substitute :sunny-day :swagger :swift :taunt :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :water-pulse :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss :selfdestruct :swagger])

(deflearnset :dp
  :mewtwo [:aerial-ace :amnesia :aqua-tail :aura-sphere :avalanche :barrier :blizzard :brick-break :bulk-up :calm-mind :charge-beam :confusion :disable :double-team :drain-punch :dream-eater :earthquake :embargo :endure :energy-ball :facade :fire-blast :fire-punch :flamethrower :flash :fling :focus-blast :focus-punch :frustration :future-sight :giga-impact :grass-knot :gravity :guard-swap :hail :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :light-screen :low-kick :magic-coat :me-first :miracle-eye :mist :mud-slap :natural-gift :poison-jab :power-swap :protect :psych-up :psychic :psycho-cut :rain-dance :recover :recycle :reflect :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :role-play :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snatch :snore :solarbeam :stone-edge :strength :substitute :sunny-day :swagger :swift :taunt :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :water-pulse :will-o-wisp :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss :selfdestruct :swagger])

(deflearnset :bw
  :mewtwo [:aerial-ace :amnesia :aqua-tail :aura-sphere :barrier :blizzard :brick-break :bulk-up :bulldoze :calm-mind :charge-beam :confusion :disable :double-team :drain-punch :dream-eater :earthquake :electro-ball :embargo :energy-ball :facade :fire-blast :fire-punch :flamethrower :flash :fling :focus-blast :foul-play :frustration :future-sight :giga-impact :grass-knot :gravity :guard-swap :hail :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :incinerate :iron-tail :light-screen :low-kick :low-sweep :magic-coat :magic-room :me-first :miracle-eye :mist :poison-jab :power-swap :protect :psych-up :psychic :psycho-cut :psyshock :psystrike :rain-dance :recover :recycle :reflect :rest :return :rock-slide :rock-smash :rock-tomb :role-play :round :safeguard :sandstorm :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :solarbeam :stone-edge :strength :substitute :sunny-day :swagger :swift :taunt :telekinesis :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :trick :trick-room :will-o-wisp :wonder-room :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss :selfdestruct :swagger])
