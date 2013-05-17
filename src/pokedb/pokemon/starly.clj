
(in-ns 'smogon.dex)


(defpokemon :starly
  :name "Starly"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:keen-eye]]
  :stats [[40 55 30 30 30 60]]
  :weight [2]
  :height [0.3])

(defpokemon :staravia
  :name "Staravia"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:intimidate :reckless]]
  :stats [[55 75 50 40 40 80]]
  :weight [15.5]
  :height [0.6])

(defpokemon :staraptor
  :name "Staraptor"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:intimidate :reckless]]
  :stats [[85 120 70 50 50 100]]
  :weight [24.9]
  :height [1.2])

(deffamily (familychain :starly :staravia :staraptor))



(deflearnset :dp
  :starly [:aerial-ace :agility :air-cutter :astonish :attract :brave-bird :captivate :defog :double-edge :double-team :endeavor :endure :facade :featherdance :fly :foresight :frustration :fury-attack :growl :heat-wave :hidden-power :mud-slap :natural-gift :ominous-wind :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :roost :sand-attack :secret-power :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :tackle :tailwind :take-down :thief :toxic :twister :u-turn :whirlwind :wing-attack]
  :staraptor [:close-combat :giga-impact :hyper-beam :sky-attack])

(deflearnset :bw
  :starly [:aerial-ace :agility :astonish :attract :brave-bird :detect :double-edge :double-team :echoed-voice :endeavor :facade :featherdance :final-gambit :fly :foresight :frustration :fury-attack :growl :heat-wave :hidden-power :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :revenge :roost :round :sand-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :tackle :tailwind :take-down :thief :toxic :u-turn :uproar :whirlwind :wing-attack :work-up]
  :staravia [:retaliate]
  :staraptor [:close-combat :giga-impact :hyper-beam :sky-attack])

