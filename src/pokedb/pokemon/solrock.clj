
(in-ns 'smogon.dex)


(defpokemon :solrock
  :name "Solrock"
  :introduced-in :rs
  :types [[:psychic :rock]]
  :egggroups [[:mineral]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[70 95 85 55 65 70]]
  :weight [154]
  :height [1.2])

(deffamily (familychain :solrock))

(deflearnset :rs
  :solrock [:body-slam :calm-mind :confusion :cosmic-power :defense-curl :double-edge :double-team :dream-eater :earthquake :endure :explosion :facade :fire-blast :fire-spin :flamethrower :flash :frustration :harden :hidden-power :hyper-beam :light-screen :mimic :overheat :protect :psych-up :psychic :psywave :reflect :rest :return :rock-slide :rock-throw :rock-tomb :rollout :safeguard :sandstorm :secret-power :selfdestruct :shadow-ball :skill-swap :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :tackle :toxic :baton-pass :body-slam :double-edge :mimic :selfdestruct])

(deflearnset :dp
  :solrock [:ancientpower :calm-mind :charge-beam :confusion :cosmic-power :double-team :dream-eater :earth-power :earthquake :embargo :endure :explosion :facade :fire-blast :fire-spin :flamethrower :flash :frustration :giga-impact :grass-knot :gravity :gyro-ball :harden :heal-block :helping-hand :hidden-power :hyper-beam :iron-defense :iron-head :light-screen :magic-coat :natural-gift :overheat :pain-split :protect :psych-up :psychic :psywave :recycle :reflect :rest :return :rock-polish :rock-slide :rock-throw :rock-tomb :rollout :safeguard :sandstorm :secret-power :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :solarbeam :stealth-rock :stone-edge :substitute :sunny-day :swagger :swift :tackle :toxic :trick-room :will-o-wisp :zen-headbutt :baton-pass :body-slam :double-edge :mimic :selfdestruct])

(deflearnset :bw
  :solrock [:acrobatics :bulldoze :calm-mind :charge-beam :confusion :cosmic-power :double-team :dream-eater :earth-power :earthquake :embargo :explosion :facade :fire-blast :fire-spin :flamethrower :flash :frustration :giga-impact :grass-knot :gravity :gyro-ball :harden :heal-block :heat-wave :helping-hand :hidden-power :hyper-beam :incinerate :iron-defense :iron-head :light-screen :magic-coat :overheat :pain-split :protect :psych-up :psychic :psyshock :psywave :recycle :reflect :rest :return :rock-polish :rock-slide :rock-throw :rock-tomb :round :safeguard :sandstorm :shadow-ball :signal-beam :skill-swap :sleep-talk :smack-down :snore :solarbeam :stealth-rock :stone-edge :substitute :sunny-day :swagger :tackle :telekinesis :toxic :trick-room :will-o-wisp :wonder-room :zen-headbutt :baton-pass :body-slam :double-edge :mimic :selfdestruct])
