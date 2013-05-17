
(in-ns 'smogon.dex)


(defpokemon :slowpoke
  :name "Slowpoke"
  :introduced-in :rb
  :types [[:psychic :water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:oblivious :own-tempo]
              :bw [:oblivious :own-tempo :regenerator]]
  :stats [[90 65 65 40 15]
          :gs [90 65 65 40 40 15]]
  :weight [36]
  :height [1.2])

(defpokemon :slowbro
  :name "Slowbro"
  :introduced-in :rb
  :types [[:psychic :water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:oblivious :own-tempo]
              :bw [:oblivious :own-tempo :regenerator]]
  :stats [[95 75 110 80 30]
          :gs [95 75 110 100 80 30]]
  :weight [78.5]
  :height [1.6])

(defpokemon :slowking
  :name "Slowking"
  :introduced-in :gs
  :types [[:psychic :water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:oblivious :own-tempo]
              :bw [:oblivious :own-tempo :regenerator]]
  :stats [[95 75 80 100 110 30]]
  :weight [79.5]
  :height [2])

(deffamily (familychain :slowpoke [:slowbro :slowking]))

(deflearnset :rb
  :slowpoke [:amnesia :bide :blizzard :body-slam :bubblebeam :confusion :dig :disable :double-edge :double-team :earthquake :fire-blast :fissure :flash :growl :headbutt :ice-beam :mimic :pay-day :psychic :psywave :rage :reflect :rest :skull-bash :strength :substitute :surf :swift :take-down :teleport :thunder-wave :toxic :tri-attack :water-gun]
  :slowbro [:counter :hyper-beam :mega-kick :mega-punch :seismic-toss :submission :withdraw])

(deflearnset :gs
  :slowpoke [:amnesia :attract :belly-drum :blizzard :confusion :curse :dig :disable :double-team :dream-eater :earthquake :endure :fire-blast :flamethrower :flash :frustration :future-sight :growl :headbutt :hidden-power :ice-beam :icy-wind :iron-tail :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :rest :return :safeguard :shadow-ball :sleep-talk :snore :stomp :strength :sunny-day :surf :swagger :swift :tackle :toxic :water-gun :zap-cannon]
  :slowbro [:dynamicpunch :fury-cutter :hyper-beam :ice-punch :rock-smash :withdraw]
  :slowking [:whirlpool])

(deflearnset :rs
  :slowpoke [:amnesia :attract :belly-drum :blizzard :body-slam :calm-mind :confusion :curse :dig :disable :dive :double-edge :double-team :dream-eater :earthquake :endure :facade :fire-blast :flamethrower :flash :frustration :future-sight :growl :hail :headbutt :hidden-power :ice-beam :icy-wind :iron-tail :mimic :mud-slap :mud-sport :nightmare :protect :psych-up :psychic :rain-dance :rest :return :safeguard :secret-power :shadow-ball :skill-swap :sleep-talk :snore :stomp :strength :substitute :sunny-day :surf :swagger :swift :tackle :thunder-wave :toxic :water-gun :water-pulse :yawn :body-slam :double-edge :icy-wind :mimic :nightmare]
  :slowbro [:brick-break :counter :dynamicpunch :focus-punch :fury-cutter :hyper-beam :ice-punch :mega-kick :mega-punch :rock-smash :seismic-toss :withdraw :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss]
  :slowking [ :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss])

(deflearnset :dp
  :slowpoke [:amnesia :aqua-tail :attract :belly-drum :blizzard :block :brine :calm-mind :captivate :confusion :curse :dig :disable :dive :double-team :dream-eater :earthquake :endure :facade :fire-blast :flamethrower :flash :frustration :future-sight :grass-knot :growl :hail :headbutt :hidden-power :ice-beam :icy-wind :iron-tail :light-screen :magic-coat :me-first :mud-slap :mud-sport :natural-gift :protect :psych-up :psychic :rain-dance :recycle :rest :return :safeguard :secret-power :shadow-ball :signal-beam :skill-swap :slack-off :sleep-talk :snore :stomp :strength :substitute :sunny-day :surf :swagger :swift :tackle :thunder-wave :toxic :trick :trick-room :water-gun :water-pulse :whirlpool :yawn :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare]
  :slowbro [:avalanche :brick-break :drain-punch :fling :focus-blast :focus-punch :fury-cutter :giga-impact :hyper-beam :ice-punch :iron-defense :rock-smash :withdraw :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss]
  :slowking [:nasty-plot :power-gem :trump-card :body-slam :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss])

(deflearnset :bw
  :slowpoke [:after-you :amnesia :aqua-tail :attract :belly-drum :blizzard :block :bulldoze :calm-mind :confusion :curse :dig :disable :dive :double-team :dream-eater :earthquake :echoed-voice :facade :fire-blast :flamethrower :flash :frustration :future-sight :grass-knot :growl :hail :headbutt :heal-pulse :hidden-power :ice-beam :icy-wind :incinerate :iron-tail :light-screen :magic-coat :me-first :mud-sport :protect :psych-up :psychic :psyshock :rain-dance :recycle :rest :return :round :safeguard :scald :shadow-ball :signal-beam :skill-swap :slack-off :sleep-talk :snore :stomp :strength :substitute :sunny-day :surf :swagger :tackle :telekinesis :thunder-wave :toxic :trick :trick-room :water-gun :water-pulse :wonder-room :yawn :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare]
  :slowbro [:aerial-ace :brick-break :drain-punch :fling :focus-blast :foul-play :giga-impact :hyper-beam :ice-punch :iron-defense :rock-smash :withdraw :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss]
  :slowking [:dragon-tail :nasty-plot :power-gem :quash :trump-card :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss])
