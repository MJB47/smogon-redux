
(in-ns 'smogon.dex)

(defpokemon :carvanha
  :name "Carvanha"
  :introduced-in :rs
  :types [[:dark :water]]
  :egggroups [[:water-2]]
  :abilities [[:rough-skin]
              :bw [:rough-skin :speed-boost]]
  :stats [[45 90 20 65 20 65]]
  :weight [20.8]
  :height [0.8])

(defpokemon :sharpedo
  :name "Sharpedo"
  :introduced-in :rs
  :types [[:dark :water]]
  :egggroups [[:water-2]]
  :abilities [[:rough-skin]
              :bw [:rough-skin :speed-boost]]
  :stats [[70 120 40 95 40 95]]
  :weight [88.8]
  :height [1.8])

(deffamily (familychain :carvanha :sharpedo))

(deflearnset :rs
  :carvanha [:agility :attract :bite :blizzard :crunch :dive :double-edge :double-team :endure :facade :focus-energy :frustration :fury-cutter :hail :hidden-power :hydro-pump :ice-beam :icy-wind :leer :mimic :mud-slap :protect :rage :rain-dance :refresh :rest :return :scary-face :screech :secret-power :sleep-talk :snore :substitute :surf :swagger :swift :take-down :taunt :thief :thrash :torment :toxic :water-pulse :waterfall]
  :sharpedo [:earthquake :hyper-beam :roar :rock-smash :rock-tomb :skull-bash :slash :strength :icy-wind :mimic])

(deflearnset :dp
  :carvanha [:agility :ancientpower :aqua-jet :assurance :attract :bite :blizzard :bounce :brine :captivate :crunch :dark-pulse :dive :double-edge :double-team :endure :facade :focus-energy :frustration :fury-cutter :hail :hidden-power :hydro-pump :ice-beam :ice-fang :icy-wind :leer :mud-slap :natural-gift :payback :protect :rage :rain-dance :refresh :rest :return :scary-face :screech :secret-power :sleep-talk :snore :spite :substitute :super-fang :surf :swagger :swift :take-down :taunt :thief :thrash :torment :toxic :uproar :water-pulse :waterfall :whirlpool :zen-headbutt]
  :sharpedo [:avalanche :earthquake :feint :giga-impact :hyper-beam :night-slash :poison-jab :roar :rock-smash :rock-tomb :skull-bash :slash :strength :icy-wind :mimic])

(deflearnset :bw
  :carvanha [:agility :ancientpower :aqua-jet :assurance :attract :bite :blizzard :bounce :brine :crunch :dark-pulse :dive :double-edge :double-team :facade :focus-energy :frustration :hail :hidden-power :hydro-pump :ice-beam :ice-fang :icy-wind :leer :payback :protect :rage :rain-dance :refresh :rest :retaliate :return :round :scald :scary-face :screech :sleep-talk :snarl :snore :spite :substitute :super-fang :surf :swagger :swift :take-down :taunt :thief :thrash :torment :toxic :uproar :water-pulse :waterfall :zen-headbutt]
  :sharpedo [:bulldoze :earthquake :feint :giga-impact :hyper-beam :night-slash :poison-jab :roar :rock-smash :rock-tomb :skull-bash :slash :strength :icy-wind :mimic])
