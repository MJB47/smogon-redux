
(in-ns 'smogon.dex)


(defpokemon :shellos
  :name "Shellos"
  :introduced-in :dp
  :types [[:water]]
  :egggroups [[:indeterminate :water-1]]
  :abilities [[:sand-force :sticky-hold :storm-drain]]
  :stats [[76 48 48 57 62 34]]
  :weight [6.3]
  :height [0.3])

(defpokemon :gastrodon
  :name "Gastrodon"
  :introduced-in :dp
  :types [[:ground :water]]
  :egggroups [[:indeterminate :water-1]]
  :abilities [[:sand-force :sticky-hold :storm-drain]]
  :stats [[111 83 68 92 82 39]]
  :weight [29.9]
  :height [0.9])

(deffamily (familychain :shellos :gastrodon))

(deflearnset :dp
  :shellos [:amnesia :ancientpower :attract :blizzard :body-slam :brine :captivate :counter :curse :dive :double-team :earth-power :endure :facade :fissure :frustration :hail :harden :headbutt :hidden-power :ice-beam :icy-wind :memento :mirror-coat :mud-bomb :mud-slap :mud-sport :muddy-water :natural-gift :pain-split :protect :rain-dance :recover :rest :return :secret-power :sleep-talk :sludge :snore :spit-up :stockpile :string-shot :substitute :surf :swagger :swallow :toxic :trump-card :water-pulse :whirlpool :yawn]
  :gastrodon [:block :dig :earthquake :flash :giga-impact :hyper-beam :rock-slide :rock-smash :rock-tomb :sandstorm :sludge-bomb :stone-edge :strength :waterfall])

(deflearnset :bw
  :shellos [:amnesia :attract :blizzard :body-slam :brine :clear-smog :counter :curse :dive :double-team :earth-power :facade :fissure :frustration :hail :harden :hidden-power :ice-beam :icy-wind :memento :mirror-coat :mist :mud-bomb :mud-slap :mud-sport :muddy-water :pain-split :protect :rain-dance :recover :rest :return :round :scald :sleep-talk :sludge :snore :spit-up :stockpile :substitute :surf :swagger :swallow :toxic :trump-card :water-pulse :yawn]
  :gastrodon [:block :bulldoze :dig :earthquake :flash :giga-impact :hyper-beam :rock-slide :rock-smash :rock-tomb :sandstorm :sludge-bomb :sludge-wave :stone-edge :strength :waterfall])
