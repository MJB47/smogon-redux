
(in-ns 'smogon.dex)

(defpokemon :tentacool
  :name "Tentacool"
  :introduced-in :rb
  :types [[:poison :water]]
  :egggroups [[:water-3]]
  :abilities [[:clear-body :liquid-ooze]
              :bw [:clear-body :liquid-ooze :rain-dish]]
  :stats [[40 40 35 100 70]
          :gs [40 40 35 50 100 70]]
  :weight [45.5]
  :height [0.9])

(defpokemon :tentacruel
  :name "Tentacruel"
  :introduced-in :rb
  :types [[:poison :water]]
  :egggroups [[:water-3]]
  :abilities [[:clear-body :liquid-ooze]
              :bw [:clear-body :liquid-ooze :rain-dish]]
  :stats [[80 70 65 120 100]
          :gs [80 70 65 80 120 100]]
  :weight [55]
  :height [1.6])

(deffamily (familychain :tentacool :tentacruel))



(deflearnset :rb
  :tentacool [:acid :barrier :bide :blizzard :bubblebeam :constrict :cut :double-edge :double-team :hydro-pump :ice-beam :mega-drain :mimic :poison-sting :rage :reflect :rest :screech :skull-bash :substitute :supersonic :surf :swords-dance :take-down :toxic :water-gun :wrap :confuse-ray]
  :tentacruel [:hyper-beam])

(deflearnset :gs
  :tentacool [:acid :attract :aurora-beam :barrier :blizzard :bubblebeam :constrict :curse :cut :double-team :endure :frustration :giga-drain :haze :hidden-power :hydro-pump :ice-beam :icy-wind :mirror-coat :poison-sting :protect :rain-dance :rapid-spin :rest :return :safeguard :screech :sleep-talk :sludge-bomb :snore :supersonic :surf :swagger :toxic :whirlpool :wrap :confuse-ray]
  :tentacruel [:hyper-beam])

(deflearnset :rs
  :tentacool [:acid :attract :aurora-beam :barrier :blizzard :bubblebeam :confuse-ray :constrict :cut :dive :double-edge :double-team :endure :facade :frustration :giga-drain :hail :haze :hidden-power :hydro-pump :ice-beam :icy-wind :mimic :mirror-coat :poison-sting :protect :rain-dance :rapid-spin :rest :return :safeguard :screech :secret-power :sleep-talk :sludge-bomb :snore :substitute :supersonic :surf :swagger :swords-dance :thief :toxic :water-pulse :waterfall :wrap :double-edge :icy-wind :mimic]
  :tentacruel [:hyper-beam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :tentacool [:acid :acupressure :attract :aurora-beam :barrier :blizzard :brine :bubblebeam :captivate :confuse-ray :constrict :cut :dive :double-team :endure :facade :frustration :giga-drain :hail :haze :hidden-power :hydro-pump :ice-beam :icy-wind :knock-off :magic-coat :mirror-coat :muddy-water :natural-gift :payback :poison-jab :poison-sting :protect :rain-dance :rapid-spin :rest :return :safeguard :screech :secret-power :sleep-talk :sludge-bomb :snore :substitute :supersonic :surf :swagger :swords-dance :thief :toxic :toxic-spikes :water-pulse :waterfall :whirlpool :wrap :wring-out :double-edge :icy-wind :mimic]
  :tentacruel [:giga-impact :hyper-beam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :tentacool [:acid :acid-spray :acupressure :aqua-ring :attract :aurora-beam :barrier :bind :blizzard :bubble :bubblebeam :confuse-ray :constrict :cut :dive :double-team :facade :frustration :giga-drain :hail :haze :hex :hidden-power :hydro-pump :ice-beam :icy-wind :knock-off :magic-coat :mirror-coat :muddy-water :payback :poison-jab :poison-sting :protect :rain-dance :rapid-spin :rest :return :round :safeguard :scald :screech :sleep-talk :sludge-bomb :sludge-wave :snore :substitute :supersonic :surf :swagger :swords-dance :thief :tickle :toxic :toxic-spikes :venoshock :water-pulse :waterfall :wrap :wring-out :double-edge :icy-wind :mimic]
  :tentacruel [:giga-impact :hyper-beam :double-edge :icy-wind :mimic])

