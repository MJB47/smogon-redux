
(in-ns 'smogon.dex)

(defpokemon :poliwag
  :name "Poliwag"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-1]]
  :abilities [[:damp :water-absorb]
              :bw [:damp :swift-swim :water-absorb]]
  :stats [[40 50 40 40 90]
          :gs [40 50 40 40 40 90]]
  :weight [12.4]
  :height [0.6])

(defpokemon :poliwhirl
  :name "Poliwhirl"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-1]]
  :abilities [[:damp :water-absorb]
              :bw [:damp :swift-swim :water-absorb]]
  :stats [[65 65 65 50 90]
          :gs [65 65 65 50 50 90]]
  :weight [20]
  :height [1])

(defpokemon :poliwrath
  :name "Poliwrath"
  :introduced-in :rb
  :types [[:fighting :water]]
  :egggroups [[:water-1]]
  :abilities [[:damp :water-absorb]
              :bw [:damp :swift-swim :water-absorb]]
  :stats [[90 85 95 70 70]
          :gs [90 85 95 70 90 70]]
  :weight [54]
  :height [1.3])

(defpokemon :politoed
  :name "Politoed"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:water-1]]
  :abilities [[:damp :water-absorb]
              :bw [:damp :drizzle :water-absorb]]
  :stats [[90 75 75 90 100 70]]
  :weight [33.9]
  :height [1.1])

(deffamily (familychain :poliwag :poliwhirl [:poliwrath :politoed]))

(deflearnset :rb
  :poliwag [:amnesia :bide :blizzard :body-slam :bubble :bubblebeam :double-edge :double-team :doubleslap :hydro-pump :hypnosis :ice-beam :mimic :psychic :psywave :rage :rest :skull-bash :substitute :surf :take-down :toxic :water-gun :growth :lovely-kiss :sweet-kiss]
  :poliwhirl [:counter :earthquake :fissure :mega-kick :mega-punch :metronome :seismic-toss :strength :submission]
  :poliwrath [:hyper-beam])

(deflearnset :gs
  :poliwag [:attract :belly-drum :blizzard :body-slam :bubble :bubblebeam :curse :defense-curl :double-team :doubleslap :endure :frustration :haze :headbutt :hidden-power :hydro-pump :hypnosis :ice-beam :icy-wind :mind-reader :mist :protect :psychic :rain-dance :rest :return :sleep-talk :snore :splash :surf :swagger :thief :toxic :water-gun :waterfall :whirlpool  :growth :lovely-kiss :sweet-kiss]
  :poliwhirl [:detect :earthquake :ice-punch :mud-slap :rock-smash :strength]
  :politoed [:dynamicpunch :hyper-beam :perish-song]
  :poliwrath [:submission])

(deflearnset :rs
  :poliwag [:attract :belly-drum :blizzard :body-slam :bubble :bubblebeam :defense-curl :dig :dive :double-edge :double-team :doubleslap :endure :facade :frustration :hail :haze :hidden-power :hydro-pump :hypnosis :ice-ball :ice-beam :icy-wind :mimic :mind-reader :mist :protect :psychic :rain-dance :rest :return :secret-power :sleep-talk :snore :splash :substitute :surf :swagger :thief :toxic :water-gun :water-pulse :water-sport :waterfall :sweet-kiss :double-edge :icy-wind :mimic]
  :poliwhirl [:brick-break :counter :earthquake :focus-punch :ice-punch :mega-kick :mega-punch :metronome :mud-slap :rock-smash :seismic-toss :strength :double-edge :icy-wind :mimic :seismic-toss]
  :politoed [:dynamicpunch :hyper-beam :perish-song :double-edge :icy-wind :mimic :seismic-toss]
  :poliwrath [:bulk-up :rock-tomb :submission :helping-hand :double-edge :helping-hand :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :poliwag [:attract :belly-drum :blizzard :body-slam :bubble :bubblebeam :captivate :dig :dive :double-team :doubleslap :encore :endeavor :endure :facade :frustration :hail :haze :headbutt :helping-hand :hidden-power :hydro-pump :hypnosis :ice-ball :ice-beam :icy-wind :mind-reader :mist :mud-bomb :mud-shot :natural-gift :protect :psychic :rain-dance :refresh :rest :return :secret-power :sleep-talk :snore :splash :substitute :surf :swagger :thief :toxic :wake-up-slap :water-gun :water-pulse :water-sport :waterfall :whirlpool :double-edge :icy-wind :mimic]
  :poliwhirl [:brick-break :earthquake :fling :focus-punch :ice-punch :mud-slap :rock-smash :strength :double-edge :icy-wind :mimic :seismic-toss]
  :politoed [:bounce :focus-blast :giga-impact :hyper-beam :hyper-voice :payback :perish-song :double-edge :icy-wind :mimic :seismic-toss]
  :poliwrath [:bulk-up :dynamicpunch :poison-jab :rock-climb :rock-slide :rock-tomb :submission :vacuum-wave :double-edge :helping-hand :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :poliwag [:attract :belly-drum :blizzard :body-slam :bubble :bubblebeam :dig :dive :double-team :doubleslap :encore :endeavor :endure :facade :frustration :hail :haze :helping-hand :hidden-power :hydro-pump :hypnosis :ice-ball :ice-beam :icy-wind :mind-reader :mist :mud-bomb :mud-shot :protect :psychic :rain-dance :refresh :rest :return :round :scald :sleep-talk :snore :splash :substitute :surf :swagger :thief :toxic :wake-up-slap :water-gun :water-pulse :water-sport :waterfall :double-edge :icy-wind :mimic]
  :poliwhirl [:brick-break :bulldoze :earthquake :fling :ice-punch :rock-smash :strength :double-edge :icy-wind :mimic :seismic-toss]
  :politoed [:bounce :echoed-voice :focus-blast :giga-impact :hyper-beam :hyper-voice :payback :perish-song :double-edge :icy-wind :mimic :seismic-toss]
  :poliwrath [:bulk-up :circle-throw :dynamicpunch :low-sweep :poison-jab :rock-slide :rock-tomb :submission :work-up :double-edge :helping-hand :icy-wind :mimic :seismic-toss])
