
(in-ns 'smogon.dex)


(defpokemon :lunatone
  :name "Lunatone"
  :introduced-in :rs
  :types [[:psychic :rock]]
  :egggroups [[:mineral]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[70 55 65 95 85 70]]
  :weight [168]
  :height [1])

(deffamily (familychain :lunatone))

(deflearnset :rs
  :lunatone [:body-slam :calm-mind :confusion :cosmic-power :defense-curl :double-edge :double-team :dream-eater :earthquake :endure :explosion :facade :flash :frustration :future-sight :harden :hidden-power :hyper-beam :hypnosis :ice-beam :light-screen :mimic :protect :psych-up :psychic :psywave :rain-dance :reflect :rest :return :rock-slide :rock-throw :rock-tomb :rollout :safeguard :sandstorm :secret-power :selfdestruct :shadow-ball :skill-swap :sleep-talk :snore :substitute :swagger :swift :tackle :toxic :baton-pass :body-slam :double-edge :mimic :selfdestruct :substitute :swagger])

(deflearnset :dp
  :lunatone [:ancientpower :blizzard :calm-mind :charge-beam :confusion :cosmic-power :double-team :dream-eater :earth-power :earthquake :embargo :endure :explosion :facade :flash :frustration :future-sight :giga-impact :grass-knot :gravity :gyro-ball :harden :heal-block :helping-hand :hidden-power :hyper-beam :hypnosis :ice-beam :iron-head :light-screen :magic-coat :natural-gift :pain-split :protect :psych-up :psychic :psywave :rain-dance :recycle :reflect :rest :return :rock-polish :rock-slide :rock-throw :rock-tomb :rollout :safeguard :sandstorm :secret-power :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :stealth-rock :stone-edge :substitute :swagger :swift :tackle :toxic :trick-room :zen-headbutt :baton-pass :body-slam :double-edge :mimic :selfdestruct :substitute :swagger])

(deflearnset :bw
  :lunatone [:acrobatics :blizzard :bulldoze :calm-mind :charge-beam :confusion :cosmic-power :double-team :dream-eater :earth-power :earthquake :embargo :explosion :facade :flash :frustration :future-sight :giga-impact :grass-knot :gravity :gyro-ball :harden :heal-block :helping-hand :hidden-power :hyper-beam :hypnosis :ice-beam :icy-wind :iron-head :light-screen :magic-coat :magic-room :pain-split :protect :psych-up :psychic :psyshock :psywave :rain-dance :recycle :reflect :rest :return :rock-polish :rock-slide :rock-throw :rock-tomb :round :safeguard :sandstorm :shadow-ball :signal-beam :skill-swap :sleep-talk :smack-down :snore :stealth-rock :stone-edge :substitute :swagger :tackle :telekinesis :toxic :trick-room :zen-headbutt :baton-pass :body-slam :double-edge :mimic :selfdestruct :substitute :swagger])
