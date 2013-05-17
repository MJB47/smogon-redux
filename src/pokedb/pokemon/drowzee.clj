
(in-ns 'smogon.dex)

(defpokemon :drowzee
  :name "Drowzee"
  :introduced-in :rb
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:insomnia]
              :dp [:forewarn :insomnia]
              :bw [:forewarn :inner-focus :insomnia]]
  :stats [[60 48 45 90 42]
          :gs [60 48 45 43 90 42]]
  :weight [32.4]
  :height [1])

(defpokemon :hypno
  :name "Hypno"
  :introduced-in :rb
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:insomnia]
              :dp [:forewarn :insomnia]
              :bw [:forewarn :inner-focus :insomnia]]
  :stats [[85 73 70 115 67]
          :gs [85 73 70 73 115 67]]
  :weight [75.6]
  :height [1.6])

(deffamily (familychain :drowzee :hypno))

(deflearnset :rb
  :drowzee [:bide :body-slam :confusion :counter :disable :double-edge :double-team :dream-eater :flash :headbutt :hypnosis :meditate :mega-kick :mega-punch :metronome :mimic :poison-gas :pound :psychic :psywave :rage :reflect :rest :seismic-toss :skull-bash :submission :substitute :take-down :teleport :thunder-wave :toxic :tri-attack]
  :hypno [:hyper-beam])

(deflearnset :gs
  :drowzee [:attract :barrier :confusion :curse :disable :double-team :dream-eater :dynamicpunch :endure :fire-punch :flash :frustration :future-sight :headbutt :hidden-power :hypnosis :ice-punch :light-screen :meditate :nightmare :poison-gas :pound :protect :psych-up :psychic :rain-dance :rest :return :shadow-ball :sleep-talk :snore :sunny-day :swagger :thunderpunch :toxic :zap-cannon]
  :hypno [:hyper-beam])

(deflearnset :rs
  :drowzee [:assist :attract :barrier :body-slam :brick-break :calm-mind :confusion :counter :disable :double-edge :double-team :dream-eater :dynamicpunch :endure :facade :fire-punch :flash :focus-punch :frustration :future-sight :headbutt :hidden-power :hypnosis :ice-punch :light-screen :meditate :mega-kick :mega-punch :metronome :mimic :nightmare :poison-gas :pound :protect :psych-up :psychic :rain-dance :reflect :rest :return :role-play :safeguard :secret-power :seismic-toss :shadow-ball :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :taunt :thief :thunder-wave :thunderpunch :torment :toxic :baton-pass :body-slam :double-edge :mimic :seismic-toss]
  :hypno [:hyper-beam])

(deflearnset :dp
  :drowzee [:assist :attract :barrier :body-slam :brick-break :calm-mind :captivate :confusion :disable :double-edge :double-team :drain-punch :dream-eater :endure :facade :fire-punch :flash :flatter :fling :focus-punch :frustration :future-sight :grass-knot :guard-swap :headbutt :hidden-power :hypnosis :ice-punch :light-screen :low-kick :magic-coat :meditate :mimic :nasty-plot :natural-gift :nightmare :poison-gas :pound :protect :psybeam :psych-up :psychic :psycho-cut :rain-dance :recycle :reflect :rest :return :role-play :safeguard :secret-power :seismic-toss :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :substitute :sunny-day :swagger :taunt :thief :thunder-wave :thunderpunch :torment :toxic :trick :trick-room :zen-headbutt :baton-pass :body-slam :double-edge :mimic :seismic-toss]
  :hypno [:focus-blast :giga-impact :hyper-beam :nightmare :switcheroo])

(deflearnset :bw
  :drowzee [:assist :attract :barrier :body-slam :brick-break :calm-mind :confusion :disable :double-edge :double-team :drain-punch :dream-eater :facade :fire-punch :flash :flatter :fling :foul-play :frustration :future-sight :grass-knot :guard-swap :headbutt :hidden-power :hypnosis :ice-punch :light-screen :low-kick :low-sweep :magic-coat :magic-room :meditate :mimic :nasty-plot :nightmare :poison-gas :pound :protect :psybeam :psych-up :psychic :psycho-cut :psyshock :rain-dance :recycle :reflect :rest :return :role-play :round :safeguard :secret-power :seismic-toss :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :substitute :sunny-day :swagger :synchronoise :taunt :telekinesis :thief :thunder-wave :thunderpunch :torment :toxic :trick :trick-room :zen-headbutt :baton-pass :body-slam :double-edge :mimic :seismic-toss]
  :hypno [:focus-blast :giga-impact :hyper-beam :nightmare :switcheroo])
