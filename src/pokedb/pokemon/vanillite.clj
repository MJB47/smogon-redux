
(in-ns 'smogon.dex)


(defpokemon :vanillish
  :name "Vanillish"
  :introduced-in :bw
  :types [[:ice]]
  :egggroups [[:mineral]]
  :abilities [[:ice-body :weak-armor]]
  :stats [[51 65 65 80 75 59]]
  :weight [41]
  :height [1.1])

(defpokemon :vanillite
  :name "Vanillite"
  :introduced-in :bw
  :types [[:ice]]
  :egggroups [[:mineral]]
  :abilities [[:ice-body :weak-armor]]
  :stats [[36 50 50 65 60 44]]
  :weight [5.7]
  :height [0.4])

(defpokemon :vanilluxe
  :name "Vanilluxe"
  :introduced-in :bw
  :types [[:ice]]
  :egggroups [[:mineral]]
  :abilities [[:ice-body :weak-armor]]
  :stats [[71 95 85 110 95 79]]
  :weight [57.5]
  :height [1.3])

(deffamily (familychain :vanillite :vanillish :vanilluxe))



(deflearnset :bw
  :vanillite [:acid-armor :astonish :attract :autotomize :avalanche :blizzard :double-team :explosion :facade :flash-cannon :frost-breath :frustration :hail :harden :hidden-power :ice-beam :ice-shard :icicle-spear :icy-wind :imprison :iron-defense :light-screen :magic-coat :magnet-rise :mirror-coat :mirror-shot :mist :natural-gift :powder-snow :protect :rain-dance :rest :return :round :sheer-cold :signal-beam :sleep-talk :snore :substitute :swagger :taunt :toxic :uproar :water-pulse]
  :vanilluxe [:giga-impact :hyper-beam :weather-ball])

