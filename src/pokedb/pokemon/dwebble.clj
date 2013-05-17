
(in-ns 'smogon.dex)


(defpokemon :dwebble
  :name "Dwebble"
  :introduced-in :bw
  :types [[:bug :rock]]
  :egggroups [[:bug :mineral]]
  :abilities [[:shell-armor :sturdy :weak-armor]]
  :stats [[50 65 85 35 35 55]]
  :weight [14.5]
  :height [0.3])

(defpokemon :crustle
  :name "Crustle"
  :introduced-in :bw
  :types [[:bug :rock]]
  :egggroups [[:bug :mineral]]
  :abilities [[:shell-armor :sturdy :weak-armor]]
  :stats [[70 95 125 65 75 45]]
  :weight [200]
  :height [1.4])

(deffamily (familychain :dwebble :crustle))

(deflearnset :bw
  :dwebble [:aerial-ace :attract :block :bug-bite :bulldoze :counter :curse :cut :dig :double-team :earthquake :endure :facade :faint-attack :flail :frustration :fury-cutter :hidden-power :hone-claws :iron-defense :knock-off :night-slash :poison-jab :protect :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :rock-wrecker :round :sand-attack :sand-tomb :sandstorm :shadow-claw :shell-smash :slash :sleep-talk :smack-down :snore :solarbeam :spikes :stealth-rock :stone-edge :strength :struggle-bug :substitute :swagger :swords-dance :toxic :withdraw :x-scissor]
  :crustle [:giga-impact :hyper-beam])
