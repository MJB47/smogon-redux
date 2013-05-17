
(in-ns 'smogon.dex)

(defpokemon :baltoy
  :name "Baltoy"
  :introduced-in :rs
  :types [[:ground :psychic]]
  :egggroups [[:mineral]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[40 40 55 40 70 55]]
  :weight [21.5]
  :height [0.5])

(defpokemon :claydol
  :name "Claydol"
  :introduced-in :rs
  :types [[:ground :psychic]]
  :egggroups [[:mineral]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[60 70 105 70 120 75]]
  :weight [108]
  :height [1.5])

(deffamily (familychain :baltoy :claydol))

(deflearnset :rs
  :baltoy [:ancientpower :confusion :cosmic-power :dig :double-edge :double-team :dream-eater :earthquake :endure :explosion :facade :flash :frustration :harden :hidden-power :ice-beam :light-screen :mimic :mud-slap :protect :psybeam :psych-up :psychic :rain-dance :rapid-spin :reflect :rest :return :rock-slide :rock-tomb :sandstorm :secret-power :selfdestruct :shadow-ball :skill-swap :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :toxic]
  :claydol [:hyper-beam :rock-smash :strength :teleport])

(deflearnset :dp
  :baltoy [:ancientpower :calm-mind :charge-beam :confusion :cosmic-power :dig :double-team :dream-eater :earth-power :earthquake :endure :explosion :facade :flash :frustration :grass-knot :gravity :gyro-ball :harden :headbutt :heal-block :hidden-power :ice-beam :light-screen :magic-coat :mud-slap :natural-gift :power-trick :protect :psybeam :psych-up :psychic :rain-dance :rapid-spin :recycle :reflect :rest :return :rock-polish :rock-slide :rock-tomb :safeguard :sandstorm :secret-power :selfdestruct :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :solarbeam :stealth-rock :substitute :sunny-day :swagger :toxic :trick :trick-room :zen-headbutt]
  :claydol [:giga-impact :hyper-beam :rock-smash :stone-edge :strength :teleport])

(deflearnset :bw
  :baltoy [:ally-switch :ancientpower :bulldoze :calm-mind :charge-beam :confusion :cosmic-power :dig :double-edge :double-team :dream-eater :drill-run :earth-power :earthquake :explosion :extrasensory :facade :flash :frustration :grass-knot :gravity :guard-split :gyro-ball :harden :heal-block :hidden-power :ice-beam :light-screen :magic-coat :mimic :mud-slap :power-split :power-trick :protect :psybeam :psych-up :psychic :psyshock :rain-dance :rapid-spin :recycle :reflect :refresh :rest :return :rock-polish :rock-slide :rock-tomb :round :safeguard :sandstorm :selfdestruct :shadow-ball :signal-beam :skill-swap :sleep-talk :smack-down :snore :solarbeam :stealth-rock :substitute :sunny-day :swagger :telekinesis :toxic :trick :trick-room :wonder-room :zen-headbutt]
  :claydol [:giga-impact :hyper-beam :rock-smash :stone-edge :strength :teleport])
