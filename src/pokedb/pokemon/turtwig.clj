
(in-ns 'smogon.dex)


(defpokemon :turtwig
  :name "Turtwig"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow :shell-armor]]
  :stats [[55 68 64 45 55 31]]
  :weight [10.2]
  :height [0.4])

(defpokemon :grotle
  :name "Grotle"
  :introduced-in :dp
  :types [[:grass]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow :shell-armor]]
  :stats [[75 89 85 55 65 36]]
  :weight [97]
  :height [1.1])

(defpokemon :torterra
  :name "Torterra"
  :introduced-in :dp
  :types [[:grass :ground]]
  :egggroups [[:monster :plant]]
  :abilities [[:overgrow :shell-armor]]
  :stats [[95 109 105 75 85 56]]
  :weight [310]
  :height [2.2])

(deffamily (familychain :turtwig :grotle :torterra))



(deflearnset :dp
  :turtwig [:absorb :amnesia :attract :bite :body-slam :bullet-seed :captivate :crunch :curse :cut :double-edge :double-team :earth-power :endure :energy-ball :facade :flash :frustration :giga-drain :grass-knot :growth :headbutt :hidden-power :iron-tail :leaf-storm :leech-seed :light-screen :mega-drain :mud-slap :natural-gift :protect :razor-leaf :reflect :rest :return :rock-climb :rock-smash :safeguard :sand-tomb :secret-power :seed-bomb :sleep-talk :snore :solarbeam :spit-up :stealth-rock :stockpile :strength :substitute :sunny-day :superpower :swagger :swallow :swords-dance :synthesis :tackle :thrash :tickle :toxic :withdraw :worry-seed]
  :torterra [:block :earthquake :frenzy-plant :giga-impact :hyper-beam :iron-head :outrage :roar :rock-polish :rock-slide :rock-tomb :sandstorm :stone-edge :wood-hammer])

(deflearnset :bw
  :turtwig [:absorb :amnesia :attract :bite :body-slam :crunch :curse :cut :double-edge :double-team :earth-power :energy-ball :facade :flash :frustration :giga-drain :grass-knot :grass-pledge :growth :hidden-power :iron-tail :leaf-storm :leech-seed :light-screen :mega-drain :protect :razor-leaf :reflect :rest :return :rock-smash :round :safeguard :sand-tomb :seed-bomb :sleep-talk :snore :solarbeam :spit-up :stealth-rock :stockpile :strength :substitute :sunny-day :superpower :swagger :swallow :swords-dance :synthesis :tackle :thrash :tickle :toxic :wide-guard :withdraw :worry-seed]
  :torterra [:block :bulldoze :earthquake :frenzy-plant :giga-impact :hyper-beam :iron-head :outrage :roar :rock-polish :rock-slide :rock-tomb :sandstorm :stone-edge :wood-hammer])

