
(in-ns 'smogon.dex)


(defpokemon :drilbur
  :name "Drilbur"
  :introduced-in :bw
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:mold-breaker :sand-force :sand-rush]]
  :stats [[60 85 40 30 45 68]]
  :weight [8.5]
  :height [0.3])

(defpokemon :excadrill
  :name "Excadrill"
  :introduced-in :bw
  :types [[:ground :steel]]
  :egggroups [[:ground]]
  :abilities [[:mold-breaker :sand-force :sand-rush]]
  :stats [[110 135 60 50 65 88]]
  :weight [40.4]
  :height [0.7])

(deffamily (familychain :drilbur :excadrill))

(deflearnset :bw
  :drilbur [:aerial-ace :attract :brick-break :bulldoze :crush-claw :cut :dig :double-team :drill-run :earth-power :earthquake :facade :fissure :fling :frustration :fury-swipes :hidden-power :hone-claws :iron-defense :metal-claw :metal-sound :mud-slap :mud-sport :poison-jab :protect :rapid-spin :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :round :sandstorm :scratch :shadow-claw :skull-bash :slash :sleep-talk :sludge-bomb :snore :stealth-rock :strength :submission :substitute :swagger :swords-dance :toxic :x-scissor]
  :excadrill [:focus-blast :giga-impact :horn-drill :hyper-beam :iron-head :magnet-rise])
