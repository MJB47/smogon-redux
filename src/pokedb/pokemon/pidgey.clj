
(in-ns 'smogon.dex)


(defpokemon :pidgey
  :name "Pidgey"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:keen-eye]
              :dp [:keen-eye :tangled-feet]
              :bw [:big-pecks :keen-eye :tangled-feet]]
  :stats [[40 45 40 35 56]
          :gs [40 45 40 35 35 56]]
  :weight [1.8]
  :height [0.3])

(defpokemon :pidgeotto
  :name "Pidgeotto"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:keen-eye]
              :dp [:keen-eye :tangled-feet]
              :bw [:big-pecks :keen-eye :tangled-feet]]
  :stats [[63 60 55 50 71]
          :gs [63 60 55 50 50 71]]
  :weight [30]
  :height [1.1])

(defpokemon :pidgeot
  :name "Pidgeot"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:keen-eye]
              :dp [:keen-eye :tangled-feet]
              :bw [:big-pecks :keen-eye :tangled-feet]]
  :stats [[83 80 75 70 91]
          :gs [83 80 75 70 70 91]]
  :weight [39.5]
  :height [1.5])

(deffamily (familychain :pidgey :pidgeotto :pidgeot))

(deflearnset :rb
  :pidgey [:agility :bide :double-edge :double-team :fly :gust :mimic :mirror-move :quick-attack :rage :razor-wind :reflect :rest :sand-attack :sky-attack :substitute :swift :take-down :toxic :whirlwind :wing-attack]
  :pidgeotto []
  :pidgeot [:hyper-beam])

(deflearnset :gs
  :pidgey [:agility :attract :curse :detect :double-team :endure :faint-attack :fly :foresight :frustration :gust :hidden-power :mirror-move :mud-slap :protect :pursuit :quick-attack :rest :return :sand-attack :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :tackle :thief :toxic :whirlwind :wing-attack]
  :pidgeotto []
  :pidgeot [:hyper-beam])

(deflearnset :rs
  :pidgey [:aerial-ace :agility :air-cutter :attract :double-edge :double-team :endure :facade :faint-attack :featherdance :fly :foresight :frustration :gust :hidden-power :mimic :mirror-move :mud-slap :protect :pursuit :quick-attack :rain-dance :rest :return :sand-attack :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :tackle :thief :toxic :whirlwind :wing-attack :double-edge :mimic :sky-attack]
  :pidgeotto [ :double-edge :mimic :refresh :sky-attack]
  :pidgeot [:hyper-beam :double-edge :mimic :sky-attack])

(deflearnset :dp
  :pidgey [:aerial-ace :agility :air-cutter :air-slash :attract :brave-bird :captivate :defog :double-team :endure :facade :faint-attack :featherdance :fly :foresight :frustration :gust :heat-wave :hidden-power :mirror-move :mud-slap :natural-gift :ominous-wind :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :roost :sand-attack :secret-power :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :tackle :tailwind :thief :toxic :twister :u-turn :uproar :whirlwind :wing-attack :double-edge :mimic :sky-attack]
  :pidgeotto [ :double-edge :mimic :refresh :sky-attack]
  :pidgeot [:giga-impact :hyper-beam :sky-attack :double-edge :mimic :sky-attack])

(deflearnset :bw
  :pidgey [:aerial-ace :agility :air-cutter :air-slash :attract :brave-bird :defog :double-team :facade :faint-attack :featherdance :fly :foresight :frustration :gust :heat-wave :hidden-power :hurricane :mirror-move :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :roost :round :sand-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :tackle :tailwind :thief :toxic :twister :u-turn :uproar :whirlwind :wing-attack :work-up :double-edge :mimic :sky-attack]
  :pidgeotto [ :double-edge :mimic :refresh :sky-attack]
  :pidgeot [:giga-impact :hyper-beam :sky-attack :double-edge :mimic :sky-attack])
