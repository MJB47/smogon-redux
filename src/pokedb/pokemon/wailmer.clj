
(in-ns 'smogon.dex)


(defpokemon :wailmer
  :name "Wailmer"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:ground :water-2]]
  :abilities [[:oblivious :water-veil]
              :bw [:oblivious :pressure :water-veil]]
  :stats [[130 70 35 70 35 60]]
  :weight [130]
  :height [2])

(defpokemon :wailord
  :name "Wailord"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:ground :water-2]]
  :abilities [[:oblivious :water-veil]
              :bw [:oblivious :pressure :water-veil]]
  :stats [[170 90 45 90 45 60]]
  :weight [398]
  :height [14.5])

(deffamily (familychain :wailmer :wailord))



(deflearnset :rs
  :wailmer [:amnesia :astonish :attract :blizzard :body-slam :curse :defense-curl :dive :double-edge :double-team :earthquake :endure :facade :fissure :frustration :growl :hail :hidden-power :hydro-pump :ice-beam :icy-wind :mimic :mist :protect :rain-dance :rest :return :roar :rock-smash :rock-tomb :rollout :secret-power :selfdestruct :sleep-talk :snore :splash :strength :substitute :surf :swagger :thrash :tickle :toxic :water-gun :water-pulse :water-spout :waterfall :whirlpool :icy-wind :mimic :selfdestruct]
  :wailord [:hyper-beam :icy-wind :mimic :selfdestruct])

(deflearnset :dp
  :wailmer [:amnesia :aqua-ring :astonish :attract :avalanche :blizzard :body-slam :bounce :brine :captivate :curse :defense-curl :dive :double-edge :double-team :earthquake :endure :facade :fissure :frustration :growl :hail :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :mist :natural-gift :protect :rain-dance :rest :return :roar :rock-smash :rock-tomb :rollout :secret-power :sleep-talk :snore :splash :strength :substitute :surf :swagger :thrash :tickle :toxic :water-gun :water-pulse :water-spout :waterfall :whirlpool :icy-wind :mimic :selfdestruct]
  :wailord [:block :giga-impact :hyper-beam :iron-head :icy-wind :mimic :selfdestruct])

(deflearnset :bw
  :wailmer [:amnesia :aqua-ring :astonish :attract :blizzard :body-slam :bounce :brine :bulldoze :curse :defense-curl :dive :double-edge :double-team :earthquake :echoed-voice :facade :fissure :frustration :growl :hail :heavy-slam :hidden-power :hydro-pump :hyper-voice :ice-beam :icy-wind :mist :protect :rain-dance :rest :return :roar :rock-smash :rock-tomb :rollout :round :scald :sleep-talk :snore :soak :splash :strength :substitute :surf :swagger :thrash :tickle :toxic :water-gun :water-pulse :water-spout :waterfall :whirlpool :zen-headbutt :icy-wind :mimic :selfdestruct]
  :wailord [:block :giga-impact :hyper-beam :iron-head :icy-wind :mimic :selfdestruct])

