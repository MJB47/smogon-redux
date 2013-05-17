
(in-ns 'smogon.dex)


(defpokemon :smoochum
  :name "Smoochum"
  :introduced-in :gs
  :types [[:ice :psychic]]
  :egggroups [[]]
  :abilities [[:oblivious]
              :dp [:forewarn :oblivious]
              :bw [:forewarn :hydration :oblivious]]
  :stats [[45 30 15 85 65 65]]
  :weight [6]
  :height [0.4])

(defpokemon :jynx
  :name "Jynx"
  :introduced-in :rb
  :types [[:ice :psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:oblivious]
              :dp [:forewarn :oblivious]
              :bw [:dry-skin :forewarn :oblivious]]
  :stats [[65 50 35 95 95]
          :gs [65 50 35 115 95 95]]
  :weight [40.6]
  :height [1.4])

(deffamily (familychain :smoochum :jynx))

(deflearnset :rb
  :jynx [:bide :blizzard :body-slam :bubblebeam :counter :double-edge :double-team :doubleslap :hyper-beam :ice-beam :ice-punch :lick :lovely-kiss :mega-kick :mega-punch :metronome :mimic :pound :psychic :psywave :rage :reflect :rest :seismic-toss :skull-bash :submission :substitute :take-down :teleport :thrash :toxic :water-gun])

(deflearnset :gs
  :smoochum [:attract :blizzard :confusion :curse :double-team :dream-eater :dynamicpunch :endure :frustration :hidden-power :ice-beam :ice-punch :icy-wind :lick :lovely-kiss :mean-look :meditate :mud-slap :nightmare :perish-song :pound :powder-snow :protect :psych-up :psychic :rain-dance :rest :return :shadow-ball :sing :sleep-talk :snore :swagger :sweet-kiss :sweet-scent :thief :toxic]
  :jynx [:body-slam :doubleslap :headbutt :hyper-beam])

(deflearnset :rs
  :smoochum [:attract :blizzard :body-slam :calm-mind :confusion :counter :double-edge :double-team :dream-eater :dynamicpunch :endure :facade :fake-out :fake-tears :flash :frustration :hail :hidden-power :ice-beam :ice-punch :icy-wind :lick :light-screen :mean-look :meditate :mega-kick :mega-punch :metronome :mimic :mud-slap :nightmare :perish-song :pound :powder-snow :protect :psych-up :psychic :rain-dance :reflect :rest :return :secret-power :seismic-toss :shadow-ball :sing :skill-swap :sleep-talk :snore :substitute :swagger :sweet-kiss :thief :toxic :water-pulse :wish :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss :substitute]
  :jynx [:brick-break :doubleslap :focus-punch :hyper-beam :lovely-kiss :taunt :torment :double-edge :icy-wind :mimic :nightmare :seismic-toss])

(deflearnset :dp
  :smoochum [:attract :avalanche :blizzard :calm-mind :captivate :confusion :copycat :double-team :dream-eater :endure :facade :fake-out :fake-tears :flash :fling :frustration :grass-knot :hail :heal-bell :helping-hand :hidden-power :ice-beam :ice-punch :icy-wind :lick :light-screen :lucky-chant :magic-coat :mean-look :meditate :miracle-eye :mud-slap :nasty-plot :natural-gift :payback :perish-song :pound :powder-snow :protect :psych-up :psychic :rain-dance :recycle :reflect :rest :return :role-play :secret-power :shadow-ball :signal-beam :sing :skill-swap :sleep-talk :snore :substitute :swagger :sweet-kiss :thief :toxic :trick :trick-room :uproar :water-pulse :wish :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss :substitute]
  :jynx [:body-slam :brick-break :doubleslap :drain-punch :energy-ball :focus-blast :focus-punch :giga-impact :headbutt :hyper-beam :lovely-kiss :taunt :torment :wake-up-slap :wring-out :double-edge :icy-wind :mimic :nightmare :seismic-toss])

(deflearnset :bw
  :smoochum [:attract :avalanche :blizzard :calm-mind :captivate :confusion :copycat :covet :double-team :dream-eater :echoed-voice :facade :fake-out :fake-tears :flash :fling :frost-breath :frustration :grass-knot :hail :heal-bell :heart-stamp :helping-hand :hidden-power :ice-beam :ice-punch :icy-wind :lick :light-screen :lucky-chant :magic-coat :magic-room :mean-look :meditate :miracle-eye :nasty-plot :payback :perish-song :pound :powder-snow :protect :psych-up :psychic :psyshock :rain-dance :recycle :reflect :rest :return :role-play :round :shadow-ball :signal-beam :sing :skill-swap :sleep-talk :snore :substitute :swagger :sweet-kiss :thief :toxic :trick :trick-room :uproar :wake-up-slap :wish :zen-headbutt :body-slam :double-edge :icy-wind :mimic :nightmare :seismic-toss :substitute]
  :jynx [:body-slam :brick-break :doubleslap :drain-punch :energy-ball :focus-blast :giga-impact :hyper-beam :hyper-voice :lovely-kiss :taunt :telekinesis :torment :wring-out :double-edge :icy-wind :mimic :nightmare :seismic-toss])
