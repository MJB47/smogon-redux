
(in-ns 'smogon.dex)


(defpokemon :skorupi
  :name "Skorupi"
  :introduced-in :dp
  :types [[:bug :poison]]
  :egggroups [[:bug :water-3]]
  :abilities [[:battle-armor :keen-eye :sniper]]
  :stats [[40 50 90 30 55 65]]
  :weight [12]
  :height [0.8])

(defpokemon :drapion
  :name "Drapion"
  :introduced-in :dp
  :types [[:dark :poison]]
  :egggroups [[:bug :water-3]]
  :abilities [[:battle-armor :keen-eye :sniper]]
  :stats [[70 90 110 60 75 95]]
  :weight [61.5]
  :height [1.3])

(deffamily (familychain :skorupi :drapion))

(deflearnset :dp
  :skorupi [:acupressure :aerial-ace :agility :aqua-tail :attract :bite :brick-break :bug-bite :captivate :confuse-ray :cross-poison :crunch :cut :dark-pulse :dig :double-team :endure :facade :faint-attack :false-swipe :flash :fling :frustration :fury-cutter :headbutt :hidden-power :iron-tail :knock-off :leer :mud-slap :natural-gift :night-slash :payback :pin-missile :poison-fang :poison-jab :poison-sting :protect :pursuit :rain-dance :rest :return :rock-smash :rock-tomb :sand-attack :scary-face :screech :secret-power :shadow-ball :slash :sleep-talk :sludge-bomb :snore :strength :substitute :sunny-day :swagger :swords-dance :taunt :thief :torment :toxic :toxic-spikes :whirlwind :x-scissor]
  :drapion [:earthquake :fire-fang :giga-impact :hyper-beam :ice-fang :roar :rock-climb :rock-slide :thunder-fang])

(deflearnset :bw
  :skorupi [:acupressure :aerial-ace :agility :aqua-tail :attract :bite :brick-break :bug-bite :confuse-ray :cross-poison :crunch :cut :dark-pulse :dig :double-team :facade :faint-attack :false-swipe :flash :fling :frustration :hidden-power :hone-claws :iron-tail :knock-off :leer :night-slash :payback :pin-missile :poison-fang :poison-jab :poison-sting :poison-tail :protect :pursuit :rain-dance :rest :return :rock-smash :rock-tomb :round :sand-attack :scary-face :screech :shadow-ball :slash :sleep-talk :sludge-bomb :snore :strength :struggle-bug :substitute :sunny-day :swagger :swords-dance :taunt :thief :torment :toxic :toxic-spikes :twineedle :venoshock :whirlwind :x-scissor]
  :drapion [:bulldoze :earthquake :fire-fang :giga-impact :hyper-beam :ice-fang :retaliate :roar :rock-slide :snarl :thunder-fang])
