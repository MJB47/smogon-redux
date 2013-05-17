
(in-ns 'smogon.dex)

(defpokemon :corphish
  :name "Corphish"
  :introduced-in :rs
  :types [[:water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:hyper-cutter :shell-armor]
              :bw [:adaptability :hyper-cutter :shell-armor]]
  :stats [[43 80 65 50 35 35]]
  :weight [11.5]
  :height [0.6])

(defpokemon :crawdaunt
  :name "Crawdaunt"
  :introduced-in :rs
  :types [[:dark :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:hyper-cutter :shell-armor]
              :bw [:adaptability :hyper-cutter :shell-armor]]
  :stats [[63 120 85 90 55 55]]
  :weight [32.8]
  :height [1.1])

(deffamily (familychain :corphish :crawdaunt))

(deflearnset :rs
  :corphish [:aerial-ace :ancientpower :attract :blizzard :body-slam :brick-break :bubble :bubblebeam :counter :crabhammer :crunch :cut :dig :double-edge :double-team :endeavor :endure :facade :frustration :fury-cutter :guillotine :hail :harden :hidden-power :ice-beam :icy-wind :knock-off :leer :mimic :mud-slap :mud-sport :protect :rain-dance :rest :return :rock-smash :rock-tomb :secret-power :sleep-talk :sludge-bomb :snore :strength :substitute :surf :swagger :swords-dance :taunt :toxic :vicegrip :water-pulse :waterfall]
  :crawdaunt [:dive :hyper-beam :swift])

(deflearnset :dp
  :corphish [:aerial-ace :ancientpower :attract :blizzard :body-slam :brick-break :bubble :bubblebeam :captivate :crabhammer :crunch :cut :dig :double-team :dragon-dance :endeavor :endure :facade :false-swipe :fling :frustration :fury-cutter :guillotine :hail :harden :hidden-power :ice-beam :icy-wind :iron-defense :knock-off :leer :metal-claw :mimic :mud-slap :mud-sport :natural-gift :night-slash :payback :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :secret-power :sleep-talk :sludge-bomb :snore :spite :strength :substitute :superpower :surf :swagger :swords-dance :taunt :toxic :vicegrip :water-pulse :waterfall :whirlpool :x-scissor]
  :crawdaunt [:avalanche :dark-pulse :dive :giga-impact :hyper-beam :swift])

(deflearnset :bw
  :corphish [:aerial-ace :ancientpower :attract :blizzard :body-slam :brick-break :bubble :bubblebeam :chip-away :crabhammer :crunch :cut :dig :double-edge :double-team :dragon-dance :endeavor :facade :false-swipe :fling :frustration :fury-cutter :guillotine :hail :harden :hidden-power :hone-claws :ice-beam :icy-wind :iron-defense :knock-off :leer :metal-claw :mimic :mud-sport :night-slash :payback :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :round :scald :sleep-talk :sludge-bomb :snore :spite :strength :substitute :superpower :surf :swagger :swords-dance :taunt :toxic :trump-card :vicegrip :waterfall :x-scissor]
  :crawdaunt [:dark-pulse :dive :giga-impact :hyper-beam :retaliate :sludge-wave :snarl :swift])
