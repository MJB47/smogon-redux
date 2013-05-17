
(in-ns 'smogon.dex)


(defpokemon :gligar
  :name "Gligar"
  :introduced-in :gs
  :types [[:flying :ground]]
  :egggroups [[:bug]]
  :abilities [[:hyper-cutter :sand-veil]
              :bw [:hyper-cutter :immunity :sand-veil]]
  :stats [[65 75 105 35 65 85]]
  :weight [64.8]
  :height [1.1])

(defpokemon :gliscor
  :name "Gliscor"
  :introduced-in :dp
  :types [[:flying :ground]]
  :egggroups [[:bug]]
  :abilities [[:hyper-cutter :poison-heal :sand-veil]]
  :stats [[75 95 125 45 75 95]]
  :weight [42.5]
  :height [2])

(deffamily (familychain :gligar :gliscor))

(deflearnset :gs
  :gligar [:attract :counter :curse :cut :detect :double-team :endure :faint-attack :frustration :fury-cutter :guillotine :harden :headbutt :hidden-power :iron-tail :metal-claw :poison-sting :protect :quick-attack :razor-wind :rest :return :rock-smash :sand-attack :sandstorm :screech :slash :sleep-talk :sludge-bomb :snore :strength :sunny-day :swagger :swift :thief :toxic :wing-attack])

(deflearnset :rs
  :gligar [:aerial-ace :attract :counter :cut :dig :double-edge :double-team :dream-eater :earthquake :endure :facade :faint-attack :frustration :fury-cutter :guillotine :harden :hidden-power :iron-tail :metal-claw :mimic :poison-sting :protect :quick-attack :rain-dance :razor-wind :rest :return :rock-slide :rock-smash :rock-tomb :sand-attack :sand-tomb :sandstorm :screech :secret-power :slash :sleep-talk :sludge-bomb :snore :steel-wing :strength :substitute :sunny-day :swagger :swift :swords-dance :thief :toxic :wing-attack :dream-eater :mimic])

(deflearnset :dp
  :gligar [:aerial-ace :agility :aqua-tail :attract :baton-pass :brick-break :captivate :counter :cross-poison :cut :dark-pulse :defog :dig :double-edge :double-team :earth-power :earthquake :endure :facade :faint-attack :false-swipe :feint :fling :frustration :fury-cutter :guillotine :harden :headbutt :hidden-power :iron-tail :knock-off :metal-claw :natural-gift :night-slash :payback :poison-jab :poison-sting :power-trick :protect :quick-attack :rain-dance :razor-wind :rest :return :rock-polish :rock-slide :rock-smash :rock-tomb :roost :sand-attack :sand-tomb :sandstorm :screech :secret-power :slash :sleep-talk :sludge-bomb :snore :stealth-rock :steel-wing :stone-edge :strength :substitute :sunny-day :swagger :swift :swords-dance :tailwind :taunt :thief :torment :toxic :u-turn :wing-attack :x-scissor :dream-eater :mimic]
  :gliscor [:fire-fang :giga-impact :hyper-beam :ice-fang :mud-slap :sky-attack :thunder-fang])

(deflearnset :bw
  :gligar [:acrobatics :aerial-ace :agility :aqua-tail :attract :baton-pass :brick-break :bug-bite :bulldoze :counter :cross-poison :cut :dark-pulse :dig :double-edge :double-team :earth-power :earthquake :facade :faint-attack :false-swipe :feint :fling :frustration :fury-cutter :guillotine :harden :hidden-power :hone-claws :iron-tail :knock-off :metal-claw :night-slash :payback :poison-jab :poison-sting :poison-tail :power-trick :protect :quick-attack :rain-dance :razor-wind :rest :return :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :roost :round :sand-attack :sand-tomb :sandstorm :screech :sky-uppercut :slash :sleep-talk :sludge-bomb :snore :stealth-rock :stone-edge :strength :struggle-bug :substitute :sunny-day :swagger :swords-dance :tailwind :taunt :thief :torment :toxic :u-turn :venoshock :wing-attack :x-scissor :dream-eater :mimic]
  :gliscor [:fire-fang :giga-impact :hyper-beam :ice-fang :sky-attack :thunder-fang])
