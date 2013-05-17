
(in-ns 'smogon.dex)


(defpokemon :qwilfish
  :name "Qwilfish"
  :introduced-in :gs
  :types [[:poison :water]]
  :egggroups [[:water-2]]
  :abilities [[:poison-point :swift-swim]
              :bw [:intimidate :poison-point :swift-swim]]
  :stats [[65 95 75 55 55 85]]
  :weight [3.9]
  :height [0.5])

(deffamily (familychain :qwilfish))

(deflearnset :gs
  :qwilfish [:attract :blizzard :bubblebeam :curse :defense-curl :double-team :endure :flail :frustration :harden :haze :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :minimize :pin-missile :poison-sting :protect :rain-dance :rest :return :rollout :sleep-talk :sludge-bomb :snore :spikes :supersonic :surf :swagger :swift :tackle :take-down :toxic :water-gun :waterfall :whirlpool])

(deflearnset :rs
  :qwilfish [:astonish :attract :blizzard :bubblebeam :defense-curl :destiny-bond :dive :double-edge :double-team :endure :facade :flail :frustration :hail :harden :haze :hidden-power :hydro-pump :ice-beam :icy-wind :mimic :minimize :pin-missile :poison-sting :protect :rain-dance :rest :return :revenge :rollout :secret-power :selfdestruct :shadow-ball :shock-wave :sleep-talk :sludge-bomb :snore :spikes :substitute :supersonic :surf :swagger :swift :swords-dance :tackle :take-down :thunder-wave :toxic :water-gun :water-pulse :waterfall :double-edge :icy-wind :mimic :selfdestruct])

(deflearnset :dp
  :qwilfish [:aqua-jet :aqua-tail :astonish :attract :blizzard :bounce :brine :bubblebeam :captivate :destiny-bond :dive :double-team :endure :explosion :facade :flail :frustration :gyro-ball :hail :harden :haze :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :minimize :natural-gift :pain-split :payback :pin-missile :poison-jab :poison-sting :protect :rain-dance :rest :return :revenge :rollout :secret-power :shadow-ball :shock-wave :signal-beam :sleep-talk :sludge-bomb :snore :spikes :spit-up :stockpile :substitute :supersonic :surf :swagger :swift :tackle :take-down :taunt :thunder-wave :toxic :toxic-spikes :water-gun :water-pulse :waterfall :whirlpool :double-edge :icy-wind :mimic :selfdestruct])

(deflearnset :bw
  :qwilfish [:acid-spray :aqua-jet :aqua-tail :astonish :attract :blizzard :bounce :brine :bubblebeam :destiny-bond :dive :double-team :explosion :facade :flail :frustration :gyro-ball :hail :harden :haze :hidden-power :hydro-pump :ice-beam :icy-wind :minimize :pain-split :payback :pin-missile :poison-jab :poison-sting :protect :rain-dance :rest :return :revenge :rollout :round :scald :shadow-ball :signal-beam :sleep-talk :sludge-bomb :sludge-wave :snore :spikes :spit-up :stockpile :substitute :supersonic :surf :swagger :tackle :take-down :taunt :thunder-wave :toxic :toxic-spikes :venoshock :water-gun :water-pulse :waterfall :double-edge :icy-wind :mimic :selfdestruct])
