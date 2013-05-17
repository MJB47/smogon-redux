
(in-ns 'smogon.dex)


(defpokemon :spoink
  :name "Spoink"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[:ground]]
  :abilities [[:own-tempo :thick-fat]
              :bw [:gluttony :own-tempo :thick-fat]]
  :stats [[60 25 35 70 80 60]]
  :weight [30.6]
  :height [0.7])

(defpokemon :grumpig
  :name "Grumpig"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[:ground]]
  :abilities [[:own-tempo :thick-fat]
              :bw [:gluttony :own-tempo :thick-fat]]
  :stats [[80 45 65 90 110 80]]
  :weight [71.5]
  :height [0.9])

(deffamily (familychain :spoink :grumpig))



(deflearnset :rs
  :spoink [:attract :body-slam :bounce :calm-mind :confuse-ray :double-edge :double-team :dream-eater :endure :extrasensory :facade :flash :frustration :future-sight :hidden-power :icy-wind :iron-tail :light-screen :magic-coat :mimic :odor-sleuth :protect :psybeam :psych-up :psychic :psywave :rain-dance :reflect :rest :return :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :splash :substitute :sunny-day :swagger :swift :taunt :thief :torment :toxic :trick :body-slam :double-edge :icy-wind :mimic]
  :grumpig [:counter :dynamicpunch :fire-punch :focus-punch :hyper-beam :ice-punch :mega-kick :mega-punch :mud-slap :seismic-toss :thunderpunch :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :spoink [:amnesia :attract :bounce :calm-mind :captivate :charge-beam :confuse-ray :double-team :dream-eater :endure :extrasensory :facade :flash :frustration :future-sight :grass-knot :headbutt :heal-bell :hidden-power :icy-wind :iron-tail :light-screen :magic-coat :mirror-coat :natural-gift :odor-sleuth :payback :power-gem :protect :psybeam :psych-up :psychic :psywave :rain-dance :recycle :reflect :rest :return :role-play :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snatch :snore :splash :substitute :sunny-day :swagger :swift :taunt :thief :thunder-wave :torment :toxic :trick :trick-room :zen-headbutt :body-slam :double-edge :icy-wind :mimic]
  :grumpig [:brick-break :drain-punch :energy-ball :fire-punch :fling :focus-blast :focus-punch :giga-impact :hyper-beam :ice-punch :mud-slap :thunderpunch :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :spoink [:amnesia :attract :bounce :calm-mind :charge-beam :confuse-ray :covet :double-team :dream-eater :endure :extrasensory :facade :flash :frustration :future-sight :grass-knot :heal-bell :hidden-power :icy-wind :iron-tail :light-screen :lucky-chant :magic-coat :mirror-coat :odor-sleuth :payback :power-gem :protect :psybeam :psych-up :psychic :psyshock :psywave :rain-dance :recycle :reflect :rest :return :role-play :round :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :splash :substitute :sunny-day :swagger :taunt :telekinesis :thief :thunder-wave :torment :toxic :trick :trick-room :whirlwind :zen-headbutt :body-slam :double-edge :icy-wind :mimic]
  :grumpig [:brick-break :bulldoze :drain-punch :energy-ball :fire-punch :fling :focus-blast :giga-impact :hyper-beam :ice-punch :thunderpunch :body-slam :double-edge :icy-wind :mimic :seismic-toss])

