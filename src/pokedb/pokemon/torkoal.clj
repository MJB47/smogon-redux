
(in-ns 'smogon.dex)


(defpokemon :torkoal
  :name "Torkoal"
  :introduced-in :rs
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:white-smoke]
              :bw [:shell-armor :white-smoke]]
  :stats [[70 85 140 85 70 20]]
  :weight [80.4]
  :height [0.5])

(deffamily (familychain :torkoal))

(deflearnset :rs
  :torkoal [:amnesia :attract :body-slam :curse :double-edge :double-team :ember :endure :eruption :explosion :facade :fire-blast :fire-spin :flail :flamethrower :frustration :heat-wave :hidden-power :iron-defense :iron-tail :mimic :mud-slap :overheat :protect :rest :return :rock-slide :rock-smash :secret-power :selfdestruct :sleep-talk :sludge-bomb :smog :smokescreen :snore :strength :substitute :sunny-day :swagger :toxic :yawn :double-edge :mimic :selfdestruct])

(deflearnset :dp
  :torkoal [:amnesia :attract :body-slam :captivate :curse :double-team :earth-power :earthquake :ember :endure :eruption :explosion :facade :fire-blast :fire-spin :fissure :flail :flamethrower :frustration :giga-impact :gyro-ball :headbutt :heat-wave :hidden-power :hyper-beam :iron-defense :iron-tail :lava-plume :mud-slap :natural-gift :overheat :protect :rapid-spin :rest :return :rock-slide :rock-smash :rock-tomb :rollout :secret-power :skull-bash :sleep-talk :sludge-bomb :smog :smokescreen :snore :solarbeam :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :toxic :will-o-wisp :withdraw :yawn :double-edge :mimic :selfdestruct])

(deflearnset :bw
  :torkoal [:after-you :amnesia :attract :body-slam :bulldoze :clear-smog :curse :double-team :earth-power :earthquake :ember :endure :eruption :explosion :facade :fire-blast :fire-spin :fissure :flail :flame-burst :flame-charge :flamethrower :frustration :giga-impact :gyro-ball :heat-wave :hidden-power :hyper-beam :incinerate :inferno :iron-defense :iron-tail :lava-plume :overheat :protect :rapid-spin :rest :return :rock-slide :rock-smash :rock-tomb :round :shell-smash :skull-bash :sleep-talk :sludge-bomb :smog :smokescreen :snore :solarbeam :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :toxic :will-o-wisp :withdraw :yawn :double-edge :mimic :selfdestruct])
