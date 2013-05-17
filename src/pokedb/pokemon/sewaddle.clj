
(in-ns 'smogon.dex)


(defpokemon :sewaddle
  :name "Sewaddle"
  :introduced-in :bw
  :types [[:bug :grass]]
  :egggroups [[:bug]]
  :abilities [[:chlorophyll :overcoat :swarm]]
  :stats [[45 53 70 40 60 42]]
  :weight [2.5]
  :height [0.3])

(defpokemon :swadloon
  :name "Swadloon"
  :introduced-in :bw
  :types [[:bug :grass]]
  :egggroups [[:bug]]
  :abilities [[:chlorophyll :leaf-guard :overcoat]]
  :stats [[55 63 90 50 80 42]]
  :weight [7.3]
  :height [0.5])

(defpokemon :leavanny
  :name "Leavanny"
  :introduced-in :bw
  :types [[:bug :grass]]
  :egggroups [[:bug]]
  :abilities [[:chlorophyll :overcoat :swarm]]
  :stats [[75 103 80 70 70 92]]
  :weight [20.5]
  :height [1.2])

(deffamily (familychain :sewaddle :swadloon :leavanny))

(deflearnset :bw
  :sewaddle [:agility :air-slash :attract :baton-pass :bug-bite :bug-buzz :calm-mind :camouflage :cut :double-team :dream-eater :electroweb :endure :energy-ball :facade :flail :flash :frustration :giga-drain :grass-knot :hidden-power :iron-defense :light-screen :magic-coat :me-first :mind-reader :payback :protect :razor-leaf :razor-wind :rest :return :round :safeguard :screech :seed-bomb :signal-beam :silver-wind :sleep-talk :snore :solarbeam :string-shot :struggle-bug :substitute :sunny-day :swagger :synthesis :tackle :toxic :worry-seed]
  :swadloon [:grasswhistle]
  :leavanny [:aerial-ace :entrainment :false-swipe :giga-impact :heal-bell :helping-hand :hone-claws :hyper-beam :knock-off :leaf-blade :leaf-storm :poison-jab :reflect :retaliate :shadow-claw :slash :swords-dance :x-scissor])
