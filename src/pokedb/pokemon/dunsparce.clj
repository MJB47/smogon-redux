
(in-ns 'smogon.dex)


(defpokemon :dunsparce
  :name "Dunsparce"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:run-away :serene-grace]
              :bw [:rattled :run-away :serene-grace]]
  :stats [[100 70 70 65 65 45]]
  :weight [14]
  :height [1.5])

(deffamily (familychain :dunsparce))

(deflearnset :gs
  :dunsparce [:ancientpower :attract :bide :bite :curse :defense-curl :dig :double-team :dream-eater :endure :flamethrower :frustration :glare :headbutt :hidden-power :iron-tail :mud-slap :nightmare :protect :psych-up :pursuit :rage :rain-dance :rest :return :rock-slide :rock-smash :rollout :screech :sleep-talk :snore :solarbeam :spite :strength :sunny-day :swagger :take-down :thief :thunder :thunderbolt :toxic :zap-cannon])

(deflearnset :rs
  :dunsparce [:ancientpower :astonish :attract :bide :bite :blizzard :body-slam :calm-mind :counter :curse :defense-curl :dig :double-edge :double-team :dream-eater :earthquake :endeavor :endure :facade :fire-blast :flail :flamethrower :frustration :glare :headbutt :hidden-power :ice-beam :iron-tail :mimic :mud-slap :nightmare :protect :psych-up :pursuit :rage :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :screech :secret-power :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :spite :strength :substitute :sunny-day :swagger :take-down :thief :thunder :thunder-wave :thunderbolt :toxic :water-pulse :yawn :body-slam :double-edge :mimic :nightmare])

(deflearnset :dp
  :dunsparce [:agility :ancientpower :aqua-tail :astonish :attract :bide :bite :blizzard :calm-mind :captivate :charge-beam :curse :defense-curl :dig :double-team :dream-eater :earthquake :endeavor :endure :facade :fire-blast :flail :flamethrower :frustration :glare :gyro-ball :headbutt :hidden-power :ice-beam :iron-tail :last-resort :magic-coat :mud-slap :natural-gift :pain-split :poison-jab :protect :psych-up :pursuit :rage :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :roost :screech :secret-power :shadow-ball :shock-wave :sleep-talk :snore :solarbeam :spite :stealth-rock :strength :substitute :sunny-day :swagger :take-down :thief :thunder :thunder-wave :thunderbolt :toxic :trump-card :water-pulse :yawn :zen-headbutt :body-slam :double-edge :mimic :nightmare])

(deflearnset :bw
  :dunsparce [:agility :ancientpower :aqua-tail :astonish :attract :bide :bind :bite :blizzard :bulldoze :calm-mind :charge-beam :coil :curse :defense-curl :dig :double-edge :double-team :dream-eater :drill-run :earthquake :endeavor :endure :facade :fire-blast :flail :flamethrower :frustration :glare :gyro-ball :headbutt :hex :hidden-power :ice-beam :incinerate :iron-tail :last-resort :magic-coat :pain-split :poison-jab :protect :psych-up :pursuit :rage :rain-dance :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :rollout :roost :round :screech :secret-power :shadow-ball :sleep-talk :snore :solarbeam :spite :stealth-rock :strength :substitute :sunny-day :swagger :take-down :thief :thunder :thunder-wave :thunderbolt :toxic :trump-card :wild-charge :yawn :zen-headbutt :body-slam :double-edge :mimic :nightmare])
