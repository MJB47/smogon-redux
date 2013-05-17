
(in-ns 'smogon.dex)


(defpokemon :skarmory
  :name "Skarmory"
  :introduced-in :gs
  :types [[:flying :steel]]
  :egggroups [[:flying]]
  :abilities [[:keen-eye :sturdy]
              :bw [:keen-eye :sturdy :weak-armor]]
  :stats [[65 80 140 40 70 70]]
  :weight [50.5]
  :height [1.7])

(deffamily (familychain :skarmory))

(deflearnset :gs
  :skarmory [:agility :attract :curse :cut :detect :double-team :drill-peck :endure :fly :frustration :fury-attack :hidden-power :leer :mud-slap :peck :protect :pursuit :rest :return :sand-attack :sandstorm :sky-attack :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :thief :toxic :whirlwind])

(deflearnset :rs
  :skarmory [:aerial-ace :agility :air-cutter :attract :counter :curse :cut :double-edge :double-team :drill-peck :endure :facade :fly :frustration :fury-attack :hidden-power :leer :metal-sound :mimic :mud-slap :peck :protect :pursuit :rest :return :roar :rock-slide :rock-smash :sand-attack :sandstorm :secret-power :sky-attack :sleep-talk :snore :spikes :steel-wing :substitute :sunny-day :swagger :swift :taunt :thief :torment :toxic :whirlwind :double-edge :mimic])

(deflearnset :dp
  :skarmory [:aerial-ace :agility :air-cutter :air-slash :assurance :attract :brave-bird :captivate :curse :cut :dark-pulse :defog :double-team :drill-peck :endure :facade :feint :flash :flash-cannon :fly :frustration :fury-attack :fury-cutter :guard-swap :hidden-power :icy-wind :iron-defense :leer :metal-sound :mud-slap :natural-gift :night-slash :ominous-wind :payback :peck :pluck :protect :pursuit :rest :return :roar :rock-slide :rock-smash :rock-tomb :roost :sand-attack :sandstorm :secret-power :sky-attack :slash :sleep-talk :snore :spikes :stealth-rock :steel-wing :substitute :sunny-day :swagger :swift :swords-dance :tailwind :taunt :thief :torment :toxic :twister :whirlwind :x-scissor :double-edge :mimic])

(deflearnset :bw
  :skarmory [:aerial-ace :agility :air-cutter :air-slash :assurance :attract :autotomize :brave-bird :curse :cut :dark-pulse :double-team :drill-peck :endure :facade :feint :flash :flash-cannon :fly :frustration :fury-attack :guard-swap :hidden-power :icy-wind :iron-defense :iron-head :leer :metal-sound :night-slash :payback :peck :pluck :protect :pursuit :rest :return :roar :rock-slide :rock-smash :rock-tomb :roost :round :sand-attack :sandstorm :sky-attack :sky-drop :slash :sleep-talk :snore :spikes :stealth-rock :steel-wing :substitute :sunny-day :swagger :swift :swords-dance :tailwind :taunt :thief :torment :toxic :whirlwind :x-scissor :double-edge :mimic])
