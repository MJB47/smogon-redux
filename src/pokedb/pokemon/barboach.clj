
(in-ns 'smogon.dex)

(defpokemon :barboach
  :name "Barboach"
  :introduced-in :rs
  :types [[:ground :water]]
  :egggroups [[:water-2]]
  :abilities [[:oblivious]
              :dp [:anticipation :oblivious]
              :bw [:anticipation :hydration :oblivious]]
  :stats [[50 48 43 46 41 60]]
  :weight [1.9]
  :height [0.4])

(defpokemon :whiscash
  :name "Whiscash"
  :introduced-in :rs
  :types [[:ground :water]]
  :egggroups [[:water-2]]
  :abilities [[:oblivious]
              :dp [:anticipation :oblivious]
              :bw [:anticipation :hydration :oblivious]]
  :stats [[110 78 73 76 71 60]]
  :weight [23.6]
  :height [0.9])

(deffamily (familychain :barboach :whiscash))

(deflearnset :rs
  :barboach [:amnesia :attract :blizzard :dive :double-edge :double-team :earthquake :endure :facade :fissure :frustration :future-sight :hail :hidden-power :ice-beam :icy-wind :magnitude :mimic :mud-slap :mud-sport :protect :rain-dance :rest :return :rock-tomb :sandstorm :secret-power :sleep-talk :snore :spark :substitute :surf :swagger :thrash :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool]
  :whiscash [:hyper-beam :rock-slide :rock-smash :strength :tickle :double-edge :icy-wind :mimic])

(deflearnset :dp
  :barboach [:amnesia :aqua-tail :attract :blizzard :bounce :captivate :dive :double-team :dragon-dance :earth-power :earthquake :endure :facade :fissure :flail :frustration :future-sight :hail :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :magnitude :mud-bomb :mud-slap :mud-sport :natural-gift :protect :rain-dance :rest :return :rock-tomb :sandstorm :secret-power :sleep-talk :snore :spark :substitute :surf :swagger :take-down :thrash :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool]
  :whiscash [:giga-impact :hyper-beam :rock-slide :rock-smash :stone-edge :strength :tickle :zen-headbutt :double-edge :icy-wind :mimic])

(deflearnset :bw
  :barboach [:amnesia :aqua-tail :attract :blizzard :bounce :bulldoze :dive :double-edge :double-team :dragon-dance :earth-power :earthquake :facade :fissure :flail :frustration :future-sight :hail :hidden-power :hydro-pump :ice-beam :icy-wind :magnitude :mimic :mud-bomb :mud-shot :mud-slap :mud-sport :muddy-water :protect :rain-dance :rest :return :rock-tomb :round :sandstorm :scald :sleep-talk :snore :spark :substitute :surf :swagger :take-down :thrash :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool]
  :whiscash [:giga-impact :hyper-beam :rock-slide :rock-smash :stone-edge :strength :tickle :zen-headbutt :double-edge :icy-wind :mimic])
