
(in-ns 'smogon.dex)


(defpokemon :roggenrola
  :name "Roggenrola"
  :introduced-in :bw
  :types [[:rock]]
  :egggroups [[:mineral]]
  :abilities [[:sand-force :sturdy]]
  :stats [[55 75 85 25 25 15]]
  :weight [18]
  :height [0.4])

(defpokemon :boldore
  :name "Boldore"
  :introduced-in :bw
  :types [[:rock]]
  :egggroups [[:mineral]]
  :abilities [[:sand-force :sturdy]]
  :stats [[70 105 105 50 40 20]]
  :weight [102]
  :height [0.9])

(defpokemon :gigalith
  :name "Gigalith"
  :introduced-in :bw
  :types [[:rock]]
  :egggroups [[:mineral]]
  :abilities [[:sand-force :sturdy]]
  :stats [[85 135 130 60 70 25]]
  :weight [260]
  :height [1.7])

(deffamily (familychain :roggenrola :boldore :gigalith))

(deflearnset :bw
  :roggenrola [:attract :autotomize :block :bulldoze :curse :double-team :earth-power :earthquake :explosion :facade :flash-cannon :frustration :gravity :harden :headbutt :heavy-slam :hidden-power :iron-defense :lock-on :magnitude :mud-slap :protect :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :round :sand-attack :sandstorm :sleep-talk :smack-down :snore :stealth-rock :stone-edge :strength :substitute :swagger :tackle :take-down :toxic]
  :boldore [:power-gem]
  :gigalith [:giga-impact :hyper-beam :iron-head :solarbeam :superpower])
