
(in-ns 'smogon.dex)


(defpokemon :weedle
  :name "Weedle"
  :introduced-in :rb
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:shield-dust]
              :bw [:run-away :shield-dust]]
  :stats [[40 35 30 20 50]
          :gs [40 35 30 20 20 50]]
  :weight [3.2]
  :height [0.3])

(defpokemon :kakuna
  :name "Kakuna"
  :introduced-in :rb
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:shed-skin]]
  :stats [[45 25 50 25 35]
          :gs [45 25 50 25 25 35]]
  :weight [10]
  :height [0.6])

(defpokemon :beedrill
  :name "Beedrill"
  :introduced-in :rb
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:swarm]
              :bw [:sniper :swarm]]
  :stats [[65 80 40 45 75]
          :gs [65 80 40 45 80 75]]
  :weight [29.5]
  :height [1])

(deffamily (familychain :weedle :kakuna :beedrill))



(deflearnset :rb
  :weedle [:poison-sting :string-shot]
  :kakuna [:harden]
  :beedrill [:agility :bide :cut :double-edge :double-team :focus-energy :fury-attack :hyper-beam :mega-drain :mimic :pin-missile :rage :reflect :rest :skull-bash :substitute :swift :swords-dance :take-down :toxic :twineedle])

(deflearnset :gs
  :weedle [:poison-sting :string-shot]
  :kakuna [:harden]
  :beedrill [:agility :attract :curse :cut :double-team :endure :focus-energy :frustration :fury-attack :fury-cutter :giga-drain :hidden-power :hyper-beam :pin-missile :protect :pursuit :rage :rest :return :sleep-talk :sludge-bomb :snore :sunny-day :swagger :sweet-scent :swift :toxic :twineedle])

(deflearnset :rs
  :weedle [:poison-sting :string-shot]
  :kakuna [:harden]
  :beedrill [:aerial-ace :agility :attract :brick-break :cut :double-edge :double-team :endeavor :endure :facade :focus-energy :frustration :fury-attack :fury-cutter :giga-drain :hidden-power :hyper-beam :mimic :pin-missile :protect :pursuit :rage :rest :return :rock-smash :secret-power :sleep-talk :sludge-bomb :snore :solarbeam :substitute :sunny-day :swagger :swift :swords-dance :thief :toxic :twineedle])

(deflearnset :dp
  :weedle [:bug-bite :poison-sting :string-shot]
  :kakuna [:harden :iron-defense]
  :beedrill [:aerial-ace :agility :air-cutter :assurance :attract :brick-break :captivate :cut :defog :double-team :endeavor :endure :facade :false-swipe :flash :focus-energy :frustration :fury-attack :fury-cutter :giga-drain :giga-impact :hidden-power :hyper-beam :knock-off :natural-gift :ominous-wind :payback :pin-missile :poison-jab :protect :pursuit :rage :rest :return :rock-smash :roost :secret-power :silver-wind :sleep-talk :sludge-bomb :snore :solarbeam :substitute :sunny-day :swagger :swift :swords-dance :tailwind :thief :toxic :toxic-spikes :twineedle :u-turn :x-scissor])

(deflearnset :bw
  :weedle [:bug-bite :electroweb :poison-sting :string-shot]
  :kakuna [:harden :iron-defense]
  :beedrill [:acrobatics :aerial-ace :agility :assurance :attract :brick-break :cut :double-team :drill-run :endeavor :facade :false-swipe :flash :focus-energy :frustration :fury-attack :giga-drain :giga-impact :hidden-power :hyper-beam :knock-off :payback :pin-missile :poison-jab :protect :pursuit :rage :rest :return :rock-smash :roost :round :sleep-talk :sludge-bomb :snore :solarbeam :struggle-bug :substitute :sunny-day :swagger :swords-dance :tailwind :thief :toxic :toxic-spikes :twineedle :u-turn :venoshock :x-scissor])

