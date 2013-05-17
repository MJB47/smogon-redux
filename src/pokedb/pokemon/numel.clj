
(in-ns 'smogon.dex)


(defpokemon :numel
  :name "Numel"
  :introduced-in :rs
  :types [[:fire :ground]]
  :egggroups [[:ground]]
  :abilities [[:oblivious]
              :dp [:oblivious :simple]
              :bw [:oblivious :own-tempo :simple]]
  :stats [[60 60 40 65 45 35]]
  :weight [24]
  :height [0.7])

(defpokemon :camerupt
  :name "Camerupt"
  :introduced-in :rs
  :types [[:fire :ground]]
  :egggroups [[:ground]]
  :abilities [[:magma-armor]
              :dp [:magma-armor :solid-rock]
              :bw [:anger-point :magma-armor :solid-rock]]
  :stats [[70 100 70 105 75 40]]
  :weight [220]
  :height [1.9])

(deffamily (familychain :numel :camerupt))

(deflearnset :rs
  :numel [:amnesia :attract :body-slam :defense-curl :dig :double-edge :double-team :earthquake :ember :endure :facade :fire-blast :flamethrower :focus-energy :frustration :growl :hidden-power :howl :magnitude :mimic :mud-slap :overheat :protect :rest :return :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :scary-face :secret-power :sleep-talk :snore :stomp :strength :substitute :sunny-day :swagger :tackle :take-down :toxic :charm :mimic]
  :camerupt [:eruption :explosion :fissure :hyper-beam :roar :selfdestruct])

(deflearnset :dp
  :numel [:amnesia :ancientpower :attract :body-slam :captivate :defense-curl :dig :double-edge :double-team :earth-power :earthquake :ember :endure :facade :fire-blast :flamethrower :focus-energy :frustration :growl :headbutt :heat-wave :hidden-power :howl :lava-plume :magnitude :mud-bomb :mud-slap :natural-gift :overheat :protect :rest :return :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :scary-face :secret-power :sleep-talk :snore :spit-up :stealth-rock :stockpile :stomp :strength :substitute :sunny-day :swagger :swallow :tackle :take-down :toxic :will-o-wisp :yawn :charm :mimic]
  :camerupt [:eruption :explosion :fissure :flash-cannon :giga-impact :hyper-beam :iron-head :roar :rock-polish :solarbeam :stone-edge])

(deflearnset :bw
  :numel [:after-you :amnesia :ancientpower :attract :body-slam :bulldoze :curse :defense-curl :dig :double-edge :double-team :earth-power :earthquake :echoed-voice :ember :endure :facade :fire-blast :flame-burst :flame-charge :flamethrower :focus-energy :frustration :growl :heat-wave :hidden-power :howl :incinerate :iron-head :lava-plume :magnitude :mud-bomb :overheat :protect :rest :return :rock-slide :rock-smash :rock-tomb :rollout :round :sandstorm :scary-face :sleep-talk :snore :spit-up :stealth-rock :stockpile :stomp :strength :substitute :sunny-day :swagger :swallow :tackle :take-down :toxic :will-o-wisp :yawn :charm :mimic]
  :camerupt [:eruption :explosion :fissure :flash-cannon :giga-impact :hyper-beam :roar :rock-polish :solarbeam :stone-edge])
