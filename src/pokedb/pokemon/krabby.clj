
(in-ns 'smogon.dex)


(defpokemon :krabby
  :name "Krabby"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-3]]
  :abilities [[:hyper-cutter :shell-armor]
              :bw [:hyper-cutter :sheer-force :shell-armor]]
  :stats [[30 105 90 25 50]
          :gs [30 105 90 25 25 50]]
  :weight [6.5]
  :height [0.4])

(defpokemon :kingler
  :name "Kingler"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-3]]
  :abilities [[:hyper-cutter :shell-armor]
              :bw [:hyper-cutter :sheer-force :shell-armor]]
  :stats [[55 130 115 50 75]
          :gs [55 130 115 50 50 75]]
  :weight [60]
  :height [1.3])

(deffamily (familychain :krabby :kingler))

(deflearnset :rb
  :krabby [:bide :blizzard :body-slam :bubble :bubblebeam :crabhammer :cut :double-edge :double-team :guillotine :harden :ice-beam :leer :mimic :rage :rest :stomp :strength :substitute :surf :swords-dance :take-down :toxic :vicegrip :water-gun]
  :kingler [:hyper-beam])

(deflearnset :gs
  :krabby [:amnesia :attract :blizzard :bubble :crabhammer :curse :cut :dig :double-team :endure :flail :frustration :fury-cutter :guillotine :harden :haze :hidden-power :ice-beam :icy-wind :leer :mud-slap :protect :rain-dance :rest :return :rock-smash :slam :sleep-talk :snore :stomp :strength :surf :swagger :thief :toxic :vicegrip :whirlpool]
  :kingler [:hyper-beam])

(deflearnset :rs
  :krabby [:amnesia :attract :blizzard :body-slam :bubble :crabhammer :cut :dig :dive :double-edge :double-team :endure :facade :flail :frustration :fury-cutter :guillotine :hail :harden :haze :hidden-power :ice-beam :icy-wind :knock-off :leer :mimic :mud-shot :mud-slap :protect :rain-dance :rest :return :rock-smash :rock-tomb :secret-power :slam :sleep-talk :snore :stomp :strength :substitute :surf :swagger :swords-dance :thief :toxic :vicegrip :water-pulse :body-slam :double-edge :icy-wind :mimic]
  :kingler [:hyper-beam :metal-claw :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :krabby [:agility :amnesia :ancientpower :attract :blizzard :brick-break :brine :bubble :bubblebeam :captivate :crabhammer :cut :dig :dive :double-team :endure :facade :false-swipe :flail :fling :frustration :fury-cutter :guillotine :hail :harden :haze :hidden-power :ice-beam :icy-wind :iron-defense :knock-off :leer :metal-claw :mud-shot :mud-slap :mud-sport :natural-gift :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :secret-power :slam :sleep-talk :snore :stomp :strength :substitute :superpower :surf :swagger :swords-dance :thief :tickle :toxic :vicegrip :water-pulse :whirlpool :x-scissor :body-slam :double-edge :icy-wind :mimic]
  :kingler [:giga-impact :hyper-beam :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :krabby [:agility :amnesia :ancientpower :attract :bide :blizzard :brick-break :brine :bubble :bubblebeam :chip-away :crabhammer :cut :dig :dive :double-team :endure :facade :false-swipe :flail :fling :frustration :guillotine :hail :harden :haze :hidden-power :hone-claws :ice-beam :icy-wind :iron-defense :knock-off :leer :metal-claw :mud-shot :mud-sport :protect :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :round :scald :slam :sleep-talk :snore :stomp :strength :substitute :superpower :surf :swagger :swords-dance :thief :tickle :toxic :vicegrip :x-scissor :body-slam :double-edge :icy-wind :mimic]
  :kingler [:giga-impact :hyper-beam :quash :wide-guard :body-slam :double-edge :icy-wind :mimic])
