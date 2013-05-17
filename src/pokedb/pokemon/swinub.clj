
(in-ns 'smogon.dex)


(defpokemon :swinub
  :name "Swinub"
  :introduced-in :gs
  :types [[:ground :ice]]
  :egggroups [[:ground]]
  :abilities [[:oblivious]
              :dp [:oblivious :snow-cloak]
              :bw [:oblivious :snow-cloak :thick-fat]]
  :stats [[50 50 40 30 30 50]]
  :weight [6.5]
  :height [0.4])

(defpokemon :piloswine
  :name "Piloswine"
  :introduced-in :gs
  :types [[:ground :ice]]
  :egggroups [[:ground]]
  :abilities [[:oblivious]
              :dp [:oblivious :snow-cloak]
              :bw [:oblivious :snow-cloak :thick-fat]]
  :stats [[100 100 80 60 60 50]]
  :weight [55.8]
  :height [1.1])

(defpokemon :mamoswine
  :name "Mamoswine"
  :introduced-in :dp
  :types [[:ground :ice]]
  :egggroups [[:ground]]
  :abilities [[:oblivious :snow-cloak :thick-fat]]
  :stats [[110 130 80 70 60 80]]
  :weight [291]
  :height [2.5])

(deffamily (familychain :swinub :piloswine :mamoswine))



(deflearnset :gs
  :swinub [:amnesia :ancientpower :attract :bite :blizzard :body-slam :curse :defense-curl :detect :double-team :earthquake :endure :frustration :headbutt :hidden-power :ice-beam :icy-wind :mist :mud-slap :powder-snow :protect :rain-dance :rest :return :roar :rock-slide :rock-smash :sleep-talk :snore :strength :swagger :tackle :take-down :toxic]
  :piloswine [:fury-attack :horn-attack :hyper-beam])

(deflearnset :rs
  :swinub [:amnesia :ancientpower :attract :bite :blizzard :body-slam :defense-curl :dig :double-edge :double-team :earthquake :endure :facade :frustration :hail :hidden-power :ice-beam :icicle-spear :icy-wind :light-screen :mimic :mist :mud-shot :mud-slap :odor-sleuth :powder-snow :protect :rain-dance :reflect :rest :return :roar :rock-slide :rock-smash :rock-tomb :sandstorm :secret-power :sleep-talk :snore :strength :substitute :swagger :tackle :take-down :toxic :charm :mimic]
  :piloswine [:fury-attack :horn-attack :hyper-beam :mimic])

(deflearnset :dp
  :swinub [:amnesia :ancientpower :attract :bite :blizzard :body-slam :captivate :curse :dig :double-edge :double-team :earth-power :earthquake :endeavor :endure :facade :fissure :frustration :hail :headbutt :hidden-power :ice-beam :ice-shard :icicle-spear :icy-wind :light-screen :mist :mud-bomb :mud-shot :mud-slap :mud-sport :natural-gift :odor-sleuth :powder-snow :protect :rain-dance :reflect :rest :return :roar :rock-slide :rock-smash :rock-tomb :sandstorm :secret-power :sleep-talk :snore :stealth-rock :strength :substitute :superpower :swagger :tackle :take-down :toxic :charm :mimic]
  :piloswine [:avalanche :fury-attack :giga-impact :hyper-beam :ice-fang :peck :stone-edge :mimic]
  :mamoswine [:block :double-hit :fury-cutter :iron-head :knock-off :rock-climb :scary-face])

(deflearnset :bw
  :swinub [:amnesia :ancientpower :attract :avalanche :bite :blizzard :body-slam :bulldoze :curse :dig :double-edge :double-team :earth-power :earthquake :endeavor :endure :facade :fissure :flail :frustration :hail :hidden-power :ice-beam :ice-shard :icicle-crash :icicle-spear :icy-wind :light-screen :mist :mud-bomb :mud-shot :mud-slap :mud-sport :odor-sleuth :powder-snow :protect :rain-dance :reflect :rest :return :roar :rock-slide :rock-smash :rock-tomb :round :sandstorm :sleep-talk :snore :stealth-rock :strength :substitute :superpower :swagger :tackle :take-down :toxic :charm :mimic]
  :piloswine [:fury-attack :giga-impact :hyper-beam :ice-fang :peck :stone-edge :thrash :mimic]
  :mamoswine [:block :double-hit :iron-head :knock-off :scary-face])

