
(in-ns 'smogon.dex)


(defpokemon :slugma
  :name "Slugma"
  :introduced-in :gs
  :types [[:fire]]
  :egggroups [[:indeterminate]]
  :abilities [[:flame-body :magma-armor]
              :bw [:flame-body :magma-armor :weak-armor]]
  :stats [[40 40 40 70 40 20]]
  :weight [35]
  :height [0.7])

(defpokemon :magcargo
  :name "Magcargo"
  :introduced-in :gs
  :types [[:fire :rock]]
  :egggroups [[:indeterminate]]
  :abilities [[:flame-body :magma-armor]
              :bw [:flame-body :magma-armor :weak-armor]]
  :stats [[50 50 120 80 80 30]]
  :weight [55]
  :height [0.8])

(deffamily (familychain :slugma :magcargo))

(deflearnset :gs
  :slugma [:acid-armor :amnesia :attract :body-slam :curse :defense-curl :double-team :ember :endure :fire-blast :flamethrower :frustration :harden :hidden-power :mud-slap :protect :rest :return :rock-slide :rock-smash :rock-throw :rollout :sleep-talk :smog :snore :sunny-day :swagger :toxic]
  :magcargo [:earthquake :hyper-beam :strength])

(deflearnset :rs
  :slugma [:acid-armor :amnesia :attract :body-slam :defense-curl :double-edge :double-team :ember :endure :facade :fire-blast :flamethrower :frustration :harden :heat-wave :hidden-power :light-screen :mimic :mud-slap :overheat :protect :reflect :rest :return :rock-slide :rock-smash :rock-throw :rollout :secret-power :selfdestruct :sleep-talk :smog :snore :substitute :sunny-day :swagger :toxic :yawn :double-edge :mimic :selfdestruct]
  :magcargo [:earthquake :hyper-beam :rock-tomb :sandstorm :strength :double-edge :mimic :refresh :selfdestruct])

(deflearnset :dp
  :slugma [:acid-armor :amnesia :ancientpower :attract :body-slam :captivate :curse :double-team :earth-power :ember :endure :facade :fire-blast :flamethrower :frustration :harden :heat-wave :hidden-power :iron-defense :lava-plume :light-screen :memento :mud-slap :natural-gift :overheat :pain-split :protect :recover :reflect :rest :return :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :secret-power :sleep-talk :smog :smokescreen :snore :spit-up :stockpile :substitute :sunny-day :swagger :swallow :toxic :will-o-wisp :yawn :double-edge :mimic :selfdestruct]
  :magcargo [:earthquake :explosion :giga-impact :gyro-ball :hyper-beam :rock-polish :sandstorm :solarbeam :stealth-rock :stone-edge :strength :double-edge :mimic :refresh :selfdestruct])

(deflearnset :bw
  :slugma [:acid-armor :after-you :amnesia :ancientpower :attract :body-slam :curse :double-team :earth-power :ember :facade :fire-blast :flame-burst :flame-charge :flamethrower :frustration :harden :heat-wave :hidden-power :incinerate :inferno :iron-defense :lava-plume :light-screen :memento :overheat :pain-split :protect :recover :reflect :rest :return :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :round :sleep-talk :smog :smokescreen :snore :spit-up :stockpile :substitute :sunny-day :swagger :swallow :toxic :will-o-wisp :yawn :double-edge :mimic :selfdestruct]
  :magcargo [:bulldoze :earthquake :explosion :giga-impact :gyro-ball :hyper-beam :rock-polish :sandstorm :shell-smash :smack-down :solarbeam :stealth-rock :stone-edge :strength :double-edge :mimic :refresh :selfdestruct])
