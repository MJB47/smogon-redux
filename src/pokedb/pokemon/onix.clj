
(in-ns 'smogon.dex)

(defpokemon :onix
  :name "Onix"
  :introduced-in :rb
  :types [[:ground :rock]]
  :egggroups [[:mineral]]
  :abilities [[:rock-head :sturdy]
              :bw [:rock-head :sturdy :weak-armor]]
  :stats [[35 45 160 30 70]
          :gs [35 45 160 30 45 70]]
  :weight [210]
  :height [8.8])

(defpokemon :steelix
  :name "Steelix"
  :introduced-in :gs
  :types [[:ground :steel]]
  :egggroups [[:mineral]]
  :abilities [[:rock-head :sturdy]
              :bw [:rock-head :sheer-force :sturdy]]
  :stats [[75 85 200 55 65 30]]
  :weight [400]
  :height [9.2])

(deffamily (familychain :onix :steelix))

(deflearnset :rb
  :onix [:bide :bind :body-slam :dig :double-edge :double-team :earthquake :explosion :fissure :harden :mimic :rage :rest :rock-slide :rock-throw :screech :selfdestruct :skull-bash :slam :strength :substitute :tackle :take-down :toxic :sharpen])

(deflearnset :gs
  :onix [:attract :bind :curse :dig :double-team :earthquake :endure :flail :frustration :harden :headbutt :hidden-power :iron-tail :mud-slap :protect :rage :rest :return :roar :rock-slide :rock-smash :rock-throw :sandstorm :screech :slam :sleep-talk :snore :strength :sunny-day :swagger :tackle :toxic :sharpen]
  :steelix [:crunch :cut :defense-curl :dragonbreath :hyper-beam :rollout])

(deflearnset :rs
  :onix [:attract :bind :block :body-slam :dig :double-edge :double-team :dragonbreath :earthquake :endure :explosion :facade :flail :frustration :harden :hidden-power :iron-tail :mimic :mud-slap :protect :psych-up :rage :rest :return :roar :rock-slide :rock-smash :rock-throw :rock-tomb :sand-tomb :sandstorm :screech :secret-power :selfdestruct :slam :sleep-talk :snore :strength :substitute :sunny-day :swagger :tackle :taunt :torment :toxic :body-slam :mimic :selfdestruct]
  :steelix [:crunch :cut :defense-curl :hyper-beam :rollout :body-slam :mimic :selfdestruct])

(deflearnset :dp
  :onix [:ancientpower :attract :bind :block :captivate :curse :defense-curl :dig :double-edge :double-team :dragon-pulse :dragonbreath :earth-power :earthquake :endure :explosion :facade :flail :flash-cannon :frustration :gyro-ball :harden :headbutt :hidden-power :iron-head :iron-tail :mud-slap :mud-sport :natural-gift :payback :protect :psych-up :rage :rest :return :roar :rock-blast :rock-climb :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :sand-tomb :sandstorm :screech :secret-power :slam :sleep-talk :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :tackle :taunt :torment :toxic :twister :body-slam :mimic :selfdestruct]
  :steelix [:aqua-tail :crunch :cut :dark-pulse :fire-fang :giga-impact :hyper-beam :ice-fang :magnet-rise :thunder-fang :body-slam :mimic :selfdestruct])

(deflearnset :bw
  :onix [:attract :bind :block :bulldoze :curse :defense-curl :dig :double-edge :double-team :dragon-pulse :dragon-tail :dragonbreath :earth-power :earthquake :explosion :facade :flail :flash-cannon :frustration :gyro-ball :harden :heavy-slam :hidden-power :iron-head :iron-tail :mud-sport :payback :protect :psych-up :rage :rest :return :roar :rock-blast :rock-climb :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :rollout :round :sand-tomb :sandstorm :screech :slam :sleep-talk :smack-down :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :tackle :taunt :torment :toxic :body-slam :mimic :selfdestruct]
  :steelix [:aqua-tail :autotomize :crunch :cut :dark-pulse :fire-fang :giga-impact :hyper-beam :ice-fang :magnet-rise :thunder-fang :body-slam :mimic :selfdestruct])
