
(in-ns 'smogon.dex)

(defpokemon :abra
  :name "Abra"
  :introduced-in :rb
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:inner-focus :synchronize]
              :bw [:inner-focus :magic-guard :synchronize]]
  :stats [[25 20 15 105 90]
          :gs [25 20 15 105 55 90]]
  :weight [19.5]
  :height [0.9])

(defpokemon :kadabra
  :name "Kadabra"
  :introduced-in :rb
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:inner-focus :synchronize]
              :bw [:inner-focus :magic-guard :synchronize]]
  :stats [[40 35 30 120 105]
          :gs [40 35 30 120 70 105]]
  :weight [56.5]
  :height [1.3])

(defpokemon :alakazam
  :name "Alakazam"
  :introduced-in :rb
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:inner-focus :synchronize]
              :bw [:inner-focus :magic-guard :synchronize]]
  :stats [[55 50 45 135 120]
          :gs [55 50 45 135 85 120]]
  :weight [48]
  :height [1.5])

(deffamily
  (familychain :abra :kadabra :alakazam))

(deflearnset :rb
  :abra [:bide :body-slam :counter :double-edge :double-team :flash :mega-kick :mega-punch :metronome :mimic :psychic :psywave :rage :reflect :rest :seismic-toss :skull-bash :submission :substitute :take-down :teleport :thunder-wave :toxic :tri-attack :foresight]
  :kadabra [:confusion :dig :disable :kinesis :psybeam :recover]
  :alakazam [:hyper-beam])

(deflearnset :gs
  :abra [:attract :barrier :curse :double-team :dream-eater :dynamicpunch :encore :endure :fire-punch :flash :frustration :headbutt :hidden-power :ice-punch :light-screen :nightmare :protect :psych-up :psychic :rain-dance :rest :return :shadow-ball :sleep-talk :snore :sunny-day :swagger :teleport :thief :thunderpunch :toxic :zap-cannon :foresight]
  :kadabra [:confusion :dig :disable :future-sight :kinesis :psybeam :recover :reflect]
  :alakazam [:hyper-beam])

(deflearnset :rs
  :abra [:attract :barrier :body-slam :calm-mind :counter :double-edge :double-team :dream-eater :dynamicpunch :encore :endure :facade :fire-punch :flash :focus-punch :frustration :hidden-power :ice-punch :iron-tail :knock-off :light-screen :mega-kick :mega-punch :metronome :mimic :nightmare :protect :psych-up :psychic :rain-dance :reflect :rest :return :safeguard :secret-power :seismic-toss :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :taunt :teleport :thief :thunder-wave :thunderpunch :torment :toxic]
  :kadabra [:confusion :disable :future-sight :kinesis :psybeam :recover :role-play :trick :body-slam :double-edge :mimic :nightmare :seismic-toss :substitute]
  :alakazam [:hyper-beam])

(deflearnset :dp
  :abra [:attract :barrier :calm-mind :captivate :charge-beam :double-team :drain-punch :dream-eater :embargo :encore :endure :energy-ball :facade :fire-punch :flash :fling :focus-punch :frustration :grass-knot :gravity :guard-swap :headbutt :hidden-power :ice-punch :iron-tail :knock-off :light-screen :magic-coat :natural-gift :power-trick :protect :psych-up :psychic :rain-dance :recycle :reflect :rest :return :role-play :safeguard :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :taunt :teleport :thief :thunder-wave :thunderpunch :torment :toxic :trick :trick-room :zen-headbutt]
  :kadabra [:confusion :disable :future-sight :kinesis :miracle-eye :psybeam :psycho-cut :recover :body-slam :double-edge :mimic :nightmare :seismic-toss :substitute]
  :alakazam [:focus-blast :giga-impact :hyper-beam])

(deflearnset :bw
  :abra [:ally-switch :attract :barrier :body-slam :calm-mind :charge-beam :double-team :double-edge :drain-punch :dream-eater :embargo :encore :energy-ball :facade :fire-punch :flash :fling :foul-play :frustration :grass-knot :gravity :guard-split :guard-swap :hidden-power :ice-punch :iron-tail :knock-off :light-screen :magic-coat :magic-room :mimic :nightmare :power-trick :protect :psych-up :psychic :psyshock :rain-dance :recycle :reflect :rest :return :role-play :round :safeguard :seismic-toss :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :taunt :telekinesis :teleport :thief :thunder-wave :thunderpunch :torment :toxic :trick :trick-room :wonder-room :zen-headbutt]
  :kadabra [:confusion :disable :future-sight :kinesis :miracle-eye :psybeam :psycho-cut :recover :body-slam :double-edge :mimic :nightmare :seismic-toss :substitute]
  :alakazam [:focus-blast :giga-impact :hyper-beam])

