
(in-ns 'smogon.dex)


(defpokemon :phanpy
  :name "Phanpy"
  :introduced-in :gs
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:pickup]
              :bw [:pickup :sand-veil]]
  :stats [[90 60 60 40 40 40]]
  :weight [33.5]
  :height [0.5])

(defpokemon :donphan
  :name "Donphan"
  :introduced-in :gs
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:sturdy]
              :bw [:sand-veil :sturdy]]
  :stats [[90 120 120 60 60 50]]
  :weight [120]
  :height [1.1])

(deffamily (familychain :phanpy :donphan))

(deflearnset :gs
  :phanpy [:ancientpower :attract :body-slam :curse :defense-curl :double-edge :double-team :earthquake :endure :flail :focus-energy :frustration :growl :headbutt :hidden-power :mud-slap :protect :rest :return :roar :rock-smash :rollout :sandstorm :sleep-talk :snore :strength :sunny-day :swagger :tackle :take-down :toxic :water-gun]
  :donphan [:fury-attack :horn-attack :hyper-beam :rapid-spin])

(deflearnset :rs
  :phanpy [:ancientpower :attract :body-slam :counter :defense-curl :double-edge :double-team :earthquake :endure :facade :fissure :flail :focus-energy :frustration :growl :hidden-power :iron-tail :mimic :mud-slap :odor-sleuth :protect :rest :return :roar :rock-smash :rock-tomb :rollout :sandstorm :secret-power :sleep-talk :snore :strength :substitute :sunny-day :swagger :tackle :take-down :toxic :mimic]
  :donphan [:fury-attack :horn-attack :hyper-beam :rapid-spin :rock-slide])

(deflearnset :dp
  :phanpy [:ancientpower :attract :body-slam :captivate :charm :counter :defense-curl :double-edge :double-team :earth-power :earthquake :endeavor :endure :facade :fissure :flail :focus-energy :frustration :growl :gunk-shot :head-smash :headbutt :hidden-power :ice-shard :iron-tail :knock-off :last-resort :mud-slap :natural-gift :odor-sleuth :protect :rest :return :roar :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :secret-power :seed-bomb :slam :sleep-talk :snore :stealth-rock :strength :substitute :sunny-day :superpower :swagger :tackle :take-down :toxic :mimic]
  :donphan [:assurance :block :bounce :fire-fang :fury-attack :giga-impact :gyro-ball :horn-attack :hyper-beam :iron-defense :magnitude :poison-jab :rapid-spin :rock-polish :scary-face :stone-edge :thunder-fang])

(deflearnset :bw
  :phanpy [:ancientpower :attract :body-slam :bulldoze :charm :counter :defense-curl :double-edge :double-team :earth-power :earthquake :echoed-voice :endeavor :endure :facade :fissure :flail :focus-energy :frustration :growl :gunk-shot :head-smash :heavy-slam :hidden-power :hyper-voice :ice-shard :iron-tail :knock-off :last-resort :mud-slap :natural-gift :odor-sleuth :protect :rest :return :roar :rock-slide :rock-smash :rock-tomb :rollout :round :sandstorm :seed-bomb :slam :sleep-talk :snore :stealth-rock :strength :substitute :sunny-day :superpower :swagger :tackle :take-down :toxic :mimic]
  :donphan [:assurance :block :bounce :fire-fang :fury-attack :giga-impact :gyro-ball :horn-attack :hyper-beam :iron-defense :magnitude :poison-jab :rapid-spin :rock-polish :scary-face :stone-edge :thunder-fang])
