
(in-ns 'smogon.dex)


(defpokemon :meditite
  :name "Meditite"
  :introduced-in :rs
  :types [[:fighting :psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:pure-power]
              :bw [:pure-power :telepathy]]
  :stats [[30 40 55 40 55 60]]
  :weight [11.2]
  :height [0.6])

(defpokemon :medicham
  :name "Medicham"
  :introduced-in :rs
  :types [[:fighting :psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:pure-power]
              :bw [:pure-power :telepathy]]
  :stats [[60 60 75 60 75 80]]
  :weight [31.5]
  :height [1.3])

(deffamily (familychain :meditite :medicham))

(deflearnset :rs
  :meditite [:attract :baton-pass :bide :body-slam :brick-break :bulk-up :calm-mind :confusion :counter :detect :double-edge :double-team :dream-eater :dynamicpunch :endure :facade :fake-out :fire-punch :flash :focus-punch :foresight :frustration :hi-jump-kick :hidden-power :ice-punch :light-screen :meditate :mega-kick :mega-punch :metronome :mimic :mind-reader :mud-slap :protect :psych-up :psychic :rain-dance :recover :reflect :rest :return :reversal :rock-smash :rock-tomb :secret-power :seismic-toss :shadow-ball :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :thunderpunch :toxic :body-slam :double-edge :mimic :seismic-toss]
  :medicham [:hyper-beam :rock-slide :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :meditite [:attract :baton-pass :bide :brick-break :bulk-up :bullet-punch :calm-mind :captivate :confusion :detect :double-team :drain-punch :dream-eater :dynamicpunch :endure :facade :fake-out :feint :fire-punch :flash :fling :focus-blast :focus-punch :force-palm :foresight :frustration :grass-knot :gravity :guard-swap :headbutt :helping-hand :hi-jump-kick :hidden-power :ice-punch :light-screen :low-kick :magic-coat :meditate :mind-reader :mud-slap :natural-gift :pain-split :poison-jab :power-swap :power-trick :protect :psych-up :psychic :psycho-cut :rain-dance :recover :recycle :reflect :rest :return :reversal :rock-slide :rock-smash :rock-tomb :role-play :secret-power :shadow-ball :signal-beam :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :thunderpunch :toxic :trick :vacuum-wave :zen-headbutt :body-slam :double-edge :mimic :seismic-toss]
  :medicham [:energy-ball :giga-impact :hyper-beam :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :meditite [:acupressure :attract :baton-pass :bide :brick-break :bulk-up :bullet-punch :calm-mind :confusion :detect :double-team :drain-punch :dream-eater :dynamicpunch :facade :fake-out :feint :fire-punch :flash :fling :focus-blast :force-palm :foresight :frustration :grass-knot :gravity :guard-swap :helping-hand :hi-jump-kick :hidden-power :ice-punch :light-screen :low-kick :low-sweep :magic-coat :meditate :mind-reader :pain-split :poison-jab :power-swap :power-trick :protect :psych-up :psychic :psycho-cut :psyshock :rain-dance :recover :recycle :reflect :rest :retaliate :return :reversal :rock-slide :rock-smash :rock-tomb :role-play :round :secret-power :shadow-ball :signal-beam :sleep-talk :snore :strength :substitute :sunny-day :swagger :telekinesis :thunderpunch :toxic :trick :work-up :zen-headbutt :body-slam :double-edge :mimic :seismic-toss]
  :medicham [:energy-ball :giga-impact :hyper-beam :body-slam :double-edge :mimic :seismic-toss])
