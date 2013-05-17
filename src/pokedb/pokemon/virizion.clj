
(in-ns 'smogon.dex)


(defpokemon :virizion
  :name "Virizion"
  :introduced-in :bw
  :types [[:fighting :grass]]
  :egggroups [[]]
  :abilities [[:justified]]
  :stats [[91 90 72 90 129 108]]
  :weight [200]
  :height [2])

(deffamily (familychain :virizion))

(deflearnset :bw
  :virizion [:aerial-ace :block :bounce :calm-mind :close-combat :cut :double-kick :double-team :energy-ball :facade :false-swipe :flash :focus-blast :frustration :giga-drain :giga-impact :grass-knot :helping-hand :hidden-power :hyper-beam :leaf-blade :leer :light-screen :magical-leaf :protect :psych-up :quick-attack :quick-guard :reflect :rest :retaliate :return :roar :rock-smash :round :sacred-sword :safeguard :seed-bomb :sleep-talk :snore :solarbeam :stone-edge :strength :substitute :sunny-day :superpower :swagger :swords-dance :synthesis :take-down :taunt :toxic :work-up :worry-seed :x-scissor :zen-headbutt])
