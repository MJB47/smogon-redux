
(in-ns 'smogon.dex)

(defpokemon :psyduck
  :name "Psyduck"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:cloud-nine :damp]
              :bw [:cloud-nine :damp :swift-swim]]
  :stats [[50 52 48 50 55]
          :gs [50 52 48 65 50 55]]
  :weight [19.6]
  :height [0.8])

(defpokemon :golduck
  :name "Golduck"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:cloud-nine :damp]
              :bw [:cloud-nine :damp :swift-swim]]
  :stats [[80 82 78 80 85]
          :gs [80 82 78 95 80 85]]
  :weight [76.6]
  :height [1.7])

(deffamily (familychain :psyduck :golduck))

(deflearnset :rb
  :psyduck [:bide :blizzard :body-slam :bubblebeam :confusion :counter :dig :disable :double-edge :double-team :fury-swipes :hydro-pump :ice-beam :mega-kick :mega-punch :mimic :pay-day :rage :rest :scratch :seismic-toss :skull-bash :strength :submission :substitute :surf :swift :tail-whip :take-down :toxic :water-gun :petal-dance :tri-attack]
  :golduck [:hyper-beam])

(deflearnset :gs
  :psyduck [:attract :blizzard :confusion :cross-chop :curse :dig :disable :double-team :dynamicpunch :endure :flash :foresight :frustration :fury-swipes :future-sight :headbutt :hidden-power :hydro-pump :hypnosis :ice-beam :ice-punch :icy-wind :iron-tail :light-screen :mud-slap :protect :psybeam :psych-up :psychic :rain-dance :rest :return :rock-smash :scratch :screech :sleep-talk :snore :strength :surf :swagger :swift :tail-whip :toxic :waterfall :whirlpool :petal-dance :tri-attack]
  :golduck [:fury-cutter :hyper-beam])

(deflearnset :rs
  :psyduck [:aerial-ace :attract :blizzard :body-slam :brick-break :calm-mind :confusion :counter :cross-chop :dig :disable :dive :double-edge :double-team :dynamicpunch :endure :facade :flash :focus-punch :foresight :frustration :fury-swipes :future-sight :hail :hidden-power :hydro-pump :hypnosis :ice-beam :ice-punch :icy-wind :iron-tail :light-screen :mega-kick :mega-punch :mimic :mud-slap :protect :psybeam :psych-up :psychic :rain-dance :refresh :rest :return :rock-smash :scratch :screech :secret-power :seismic-toss :sleep-talk :snore :strength :substitute :surf :swagger :swift :tail-whip :toxic :water-pulse :water-sport :waterfall :mud-sport :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :golduck [:fury-cutter :hyper-beam :charm :body-slam :brick-break :charm :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :psyduck [:aerial-ace :amnesia :aqua-tail :attract :blizzard :brick-break :brine :calm-mind :captivate :confuse-ray :confusion :cross-chop :dig :disable :dive :double-team :encore :endure :facade :flash :fling :focus-punch :foresight :frustration :fury-swipes :future-sight :hail :headbutt :hidden-power :hydro-pump :hypnosis :ice-beam :ice-punch :icy-wind :iron-tail :light-screen :mud-bomb :mud-slap :natural-gift :protect :psybeam :psych-up :psychic :rain-dance :refresh :rest :return :rock-smash :role-play :scratch :screech :secret-power :shadow-claw :signal-beam :sleep-talk :snore :strength :substitute :surf :swagger :swift :tail-whip :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool :worry-seed :yawn :zen-headbutt :mud-sport :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :golduck [:aqua-jet :focus-blast :fury-cutter :giga-impact :hyper-beam :low-kick :rock-climb :charm :body-slam :brick-break :charm :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :psyduck [:aerial-ace :amnesia :aqua-tail :attract :blizzard :brick-break :calm-mind :confuse-ray :confusion :cross-chop :dig :disable :dive :double-team :encore :facade :flash :fling :foresight :frustration :fury-swipes :future-sight :hail :hidden-power :hone-claws :hydro-pump :hypnosis :ice-beam :ice-punch :icy-wind :iron-tail :light-screen :mud-bomb :protect :psybeam :psych-up :psychic :psyshock :rain-dance :refresh :rest :return :rock-smash :role-play :round :scald :scratch :screech :secret-power :shadow-claw :signal-beam :sleep-talk :snore :soak :strength :substitute :surf :swagger :synchronoise :tail-whip :telekinesis :toxic :water-gun :water-pulse :water-sport :waterfall :wonder-room :worry-seed :yawn :zen-headbutt :mud-sport :body-slam :double-edge :icy-wind :mimic :seismic-toss]
  :golduck [:aqua-jet :focus-blast :giga-impact :hyper-beam :low-kick :low-sweep :charm :body-slam :brick-break :charm :double-edge :icy-wind :mimic :seismic-toss])
