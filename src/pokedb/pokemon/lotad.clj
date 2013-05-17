
(in-ns 'smogon.dex)


(defpokemon :lotad
  :name "Lotad"
  :introduced-in :rs
  :types [[:grass :water]]
  :egggroups [[:plant :water-1]]
  :abilities [[:rain-dish :swift-swim]
              :bw [:own-tempo :rain-dish :swift-swim]]
  :stats [[40 30 30 40 50 30]]
  :weight [2.6]
  :height [0.5])

(defpokemon :lombre
  :name "Lombre"
  :introduced-in :rs
  :types [[:grass :water]]
  :egggroups [[:plant :water-1]]
  :abilities [[:rain-dish :swift-swim]
              :bw [:own-tempo :rain-dish :swift-swim]]
  :stats [[60 50 50 60 70 50]]
  :weight [32.5]
  :height [1.2])

(defpokemon :ludicolo
  :name "Ludicolo"
  :introduced-in :rs
  :types [[:grass :water]]
  :egggroups [[:plant :water-1]]
  :abilities [[:rain-dish :swift-swim]
              :bw [:own-tempo :rain-dish :swift-swim]]
  :stats [[80 70 70 90 100 70]]
  :weight [55]
  :height [1.5])

(deffamily (familychain :lotad :lombre :ludicolo))

(deflearnset :rs
  :lotad [:absorb :astonish :attract :blizzard :body-slam :bullet-seed :double-edge :double-team :endure :facade :flail :flash :frustration :giga-drain :growl :hail :hidden-power :ice-beam :icy-wind :leech-seed :mega-drain :mimic :mist :nature-power :protect :rain-dance :razor-leaf :rest :return :secret-power :sleep-talk :snore :solarbeam :substitute :sunny-day :surf :swagger :sweet-scent :swords-dance :synthesis :thief :toxic :water-gun :water-pulse :body-slam :double-edge :icy-wind :mimic]
  :lombre [:brick-break :dive :dynamicpunch :fake-out :fire-punch :fury-swipes :hydro-pump :ice-punch :mud-slap :rock-smash :strength :thunderpunch :uproar :water-sport :waterfall :body-slam :double-edge :icy-wind :mimic]
  :ludicolo [:counter :focus-punch :hyper-beam :mega-kick :mega-punch :metronome :seismic-toss :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :lotad [:absorb :astonish :attract :blizzard :bubblebeam :bullet-seed :captivate :counter :double-team :endure :energy-ball :facade :flail :flash :frustration :giga-drain :grass-knot :growl :hail :headbutt :hidden-power :ice-beam :icy-wind :leech-seed :mega-drain :mist :natural-gift :nature-power :protect :rain-dance :razor-leaf :rest :return :secret-power :seed-bomb :sleep-talk :snore :solarbeam :substitute :sunny-day :surf :swagger :sweet-scent :swords-dance :synthesis :thief :tickle :toxic :uproar :water-gun :water-pulse :whirlpool :zen-headbutt :body-slam :double-edge :icy-wind :mimic]
  :lombre [:brick-break :dive :drain-punch :fake-out :fire-punch :fling :fury-swipes :hydro-pump :ice-punch :mud-slap :rock-smash :strength :thunderpunch :water-sport :waterfall :body-slam :double-edge :icy-wind :mimic]
  :ludicolo [:focus-blast :focus-punch :giga-impact :hyper-beam :rock-climb :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :lotad [:absorb :astonish :attract :blizzard :bubblebeam :counter :double-team :echoed-voice :energy-ball :facade :flail :flash :frustration :giga-drain :grass-knot :growl :hail :hidden-power :ice-beam :icy-wind :leech-seed :mega-drain :mist :natural-gift :nature-power :protect :rain-dance :razor-leaf :rest :return :round :scald :seed-bomb :sleep-talk :snore :solarbeam :substitute :sunny-day :surf :swagger :sweet-scent :swords-dance :synthesis :teeter-dance :thief :tickle :toxic :uproar :water-gun :zen-headbutt :body-slam :double-edge :icy-wind :mimic]
  :lombre [:brick-break :dive :drain-punch :fake-out :fire-punch :fling :fury-swipes :hone-claws :hydro-pump :hyper-voice :ice-punch :rock-smash :strength :thunderpunch :water-sport :waterfall :body-slam :double-edge :icy-wind :mimic]
  :ludicolo [:focus-blast :giga-impact :hyper-beam :body-slam :double-edge :icy-wind :mimic :seismic-toss])
