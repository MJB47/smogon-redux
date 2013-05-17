
(in-ns 'smogon.dex)


(defpokemon :burmy
  :name "Burmy"
  :introduced-in :dp
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:overcoat :shed-skin]]
  :stats [[40 29 45 29 45 36]]
  :weight [3.4]
  :height [0.2])

(defpokemon :wormadam
  :name "Wormadam"
  :introduced-in :dp
  :types [[:bug :grass]]
  :egggroups [[:bug]]
  :abilities [[:anticipation :overcoat]]
  :stats [[60 59 85 79 105 36]]
  :weight [6.5]
  :height [0.5])

(defpokemon :wormadam-s
  :name "Wormadam-Sandy"
  :introduced-in :dp
  :types [[:bug :ground]]
  :egggroups [[:bug]]
  :abilities [[:anticipation :overcoat]]
  :stats [[60 79 105 59 85 36]]
  :weight [6.5]
  :height [0.5])

(defpokemon :wormadam-t
  :name "Wormadam-Trash"
  :introduced-in :dp
  :types [[:bug :steel]]
  :egggroups [[:bug]]
  :abilities [[:anticipation :overcoat]]
  :stats [[60 69 95 69 95 36]]
  :weight [6.5]
  :height [0.5])

(defpokemon :mothim
  :name "Mothim"
  :introduced-in :dp
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:swarm :tinted-lens]]
  :stats [[70 94 50 94 50 66]]
  :weight [23.3]
  :height [0.9])

(deffamily (familychain :burmy [:wormadam :mothim]))

(deflearnset :dp
  :burmy [:bug-bite :hidden-power :protect :snore :string-shot :tackle]
  :mothim [:aerial-ace :air-cutter :air-slash :attract :bug-buzz :camouflage :captivate :confusion :defog :double-team :dream-eater :endure :energy-ball :facade :flash :frustration :giga-drain :giga-impact :gust :hyper-beam :mud-slap :natural-gift :ominous-wind :poisonpowder :psybeam :psych-up :psychic :rain-dance :rest :return :roost :safeguard :secret-power :shadow-ball :signal-beam :silver-wind :skill-swap :sleep-talk :solarbeam :substitute :sunny-day :swagger :swift :tailwind :thief :toxic :twister :u-turn]
  :wormadam [:bullet-seed :endeavor :flail :grass-knot :growth :leaf-storm :razor-leaf :seed-bomb :sucker-punch :synthesis :uproar :worry-seed])

(deflearnset :bw
  :burmy [:bug-bite :electroweb :hidden-power :protect :snore :tackle]
  :mothim [:acrobatics :aerial-ace :air-slash :attract :bug-buzz :camouflage :confusion :double-team :dream-eater :energy-ball :facade :flash :frustration :giga-drain :giga-impact :gust :hyper-beam :poisonpowder :psybeam :psych-up :psychic :quiver-dance :rain-dance :rest :return :roost :round :safeguard :shadow-ball :signal-beam :silver-wind :skill-swap :sleep-talk :solarbeam :struggle-bug :substitute :sunny-day :swagger :tailwind :thief :toxic :u-turn :venoshock]
  :wormadam [:captivate :endeavor :flail :grass-knot :growth :leaf-storm :razor-leaf :seed-bomb :synthesis :uproar :worry-seed])
