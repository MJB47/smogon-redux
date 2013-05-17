
(in-ns 'smogon.dex)


(defpokemon :relicanth
  :name "Relicanth"
  :introduced-in :rs
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-2]]
  :abilities [[:rock-head :swift-swim]
              :bw [:rock-head :sturdy :swift-swim]]
  :stats [[100 90 130 45 65 55]]
  :weight [23.4]
  :height [1])

(deffamily (familychain :relicanth))

(deflearnset :rs
  :relicanth [:amnesia :ancientpower :attract :blizzard :body-slam :calm-mind :dive :double-edge :double-team :earthquake :endure :facade :frustration :hail :harden :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :magnitude :mimic :mud-slap :mud-sport :protect :psych-up :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :safeguard :sandstorm :secret-power :skull-bash :sleep-talk :snore :substitute :surf :swagger :tackle :take-down :toxic :water-gun :water-pulse :water-sport :waterfall :yawn :body-slam :icy-wind :mimic])

(deflearnset :dp
  :relicanth [:amnesia :ancientpower :aqua-tail :attract :blizzard :bounce :brine :calm-mind :captivate :dive :double-edge :double-team :earth-power :earthquake :endure :facade :frustration :giga-impact :hail :harden :head-smash :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :magnitude :mud-slap :mud-sport :muddy-water :natural-gift :protect :psych-up :rain-dance :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :safeguard :sandstorm :secret-power :skull-bash :sleep-talk :snore :stealth-rock :stone-edge :substitute :surf :swagger :tackle :take-down :toxic :water-gun :water-pulse :water-sport :waterfall :whirlpool :yawn :body-slam :icy-wind :mimic])

(deflearnset :bw
  :relicanth [:amnesia :ancientpower :aqua-tail :attract :blizzard :bounce :brine :bulldoze :calm-mind :dive :double-edge :double-team :earth-power :earthquake :facade :frustration :giga-impact :hail :harden :head-smash :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :magnitude :mud-shot :mud-slap :mud-sport :muddy-water :protect :psych-up :rain-dance :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :round :safeguard :sandstorm :scald :skull-bash :sleep-talk :smack-down :snore :stealth-rock :stone-edge :substitute :surf :swagger :tackle :take-down :toxic :water-gun :water-sport :waterfall :yawn :zen-headbutt :body-slam :icy-wind :mimic])
