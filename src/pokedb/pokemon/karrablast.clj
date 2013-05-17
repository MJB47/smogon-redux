
(in-ns 'smogon.dex)


(defpokemon :karrablast
  :name "Karrablast"
  :introduced-in :bw
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:no-guard :shed-skin :swarm]]
  :stats [[50 75 45 40 45 60]]
  :weight [5.9]
  :height [0.5])

(defpokemon :escavalier
  :name "Escavalier"
  :introduced-in :bw
  :types [[:bug :steel]]
  :egggroups [[:bug]]
  :abilities [[:overcoat :shell-armor :swarm]]
  :stats [[70 135 105 60 105 20]]
  :weight [33]
  :height [1])

(deffamily (familychain :karrablast :escavalier))

(deflearnset :bw
  :karrablast [:aerial-ace :attract :bug-bite :bug-buzz :counter :cut :double-edge :double-team :endure :energy-ball :facade :faint-attack :false-swipe :flail :frustration :fury-attack :fury-cutter :giga-drain :headbutt :hidden-power :horn-attack :iron-defense :knock-off :leer :megahorn :peck :poison-jab :protect :pursuit :rain-dance :rest :return :round :scary-face :screech :slash :sleep-talk :snore :struggle-bug :substitute :swagger :swords-dance :take-down :toxic :x-scissor]
  :escavalier [:focus-blast :giga-impact :hyper-beam :iron-head :quick-guard :reversal :rock-smash :twineedle])
