
(in-ns 'smogon.dex)


(defpokemon :hippopotas
  :name "Hippopotas"
  :introduced-in :dp
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:sand-force :sand-stream]]
  :stats [[68 72 78 38 42 32]]
  :weight [49.5]
  :height [0.8])

(defpokemon :hippowdon
  :name "Hippowdon"
  :introduced-in :dp
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:sand-force :sand-stream]]
  :stats [[108 112 118 68 72 47]]
  :weight [300]
  :height [2])

(deffamily (familychain :hippopotas :hippowdon))

(deflearnset :dp
  :hippopotas [:attract :bite :body-slam :captivate :crunch :curse :dig :double-edge :double-team :earth-power :earthquake :endure :facade :fissure :frustration :headbutt :hidden-power :iron-tail :mud-slap :natural-gift :protect :rest :return :revenge :roar :rock-slide :rock-smash :rock-tomb :sand-attack :sand-tomb :sandstorm :secret-power :slack-off :sleep-talk :snore :spit-up :stealth-rock :stockpile :strength :substitute :sunny-day :superpower :swagger :swallow :tackle :take-down :toxic :water-pulse :yawn]
  :hippowdon [:fire-fang :giga-impact :hyper-beam :ice-fang :iron-head :stone-edge :thunder-fang])

(deflearnset :bw
  :hippopotas [:attract :bite :body-slam :bulldoze :crunch :curse :dig :double-edge :double-team :earth-power :earthquake :facade :fissure :frustration :hidden-power :iron-tail :protect :rest :return :revenge :roar :rock-slide :rock-smash :rock-tomb :round :sand-attack :sand-tomb :sandstorm :slack-off :sleep-talk :snore :spit-up :stealth-rock :stockpile :strength :substitute :sunny-day :superpower :swagger :swallow :tackle :take-down :toxic :whirlwind :yawn]
  :hippowdon [:fire-fang :giga-impact :hyper-beam :ice-fang :iron-head :stone-edge :thunder-fang])
