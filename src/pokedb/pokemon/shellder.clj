
(in-ns 'smogon.dex)


(defpokemon :shellder
  :name "Shellder"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:water-3]]
  :abilities [[:shell-armor]
              :dp [:shell-armor :skill-link]
              :bw [:overcoat :shell-armor :skill-link]]
  :stats [[30 65 100 45 40]
          :gs [30 65 100 45 25 40]]
  :weight [4]
  :height [0.3])

(defpokemon :cloyster
  :name "Cloyster"
  :introduced-in :rb
  :types [[:ice :water]]
  :egggroups [[:water-3]]
  :abilities [[:shell-armor]
              :dp [:shell-armor :skill-link]
              :bw [:overcoat :shell-armor :skill-link]]
  :stats [[50 95 180 85 70]
          :gs [50 95 180 85 45 70]]
  :weight [132.5]
  :height [1.5])

(deffamily (familychain :shellder :cloyster))

(deflearnset :rb
  :shellder [:aurora-beam :bide :blizzard :bubblebeam :clamp :double-edge :double-team :explosion :ice-beam :leer :mimic :rage :reflect :rest :selfdestruct :substitute :supersonic :surf :swift :tackle :take-down :teleport :toxic :tri-attack :water-gun :withdraw]
  :cloyster [:hyper-beam :spike-cannon])

(deflearnset :gs
  :shellder [:attract :aurora-beam :barrier :blizzard :bubblebeam :clamp :curse :double-team :endure :frustration :hidden-power :ice-beam :icy-wind :leer :protect :rain-dance :rapid-spin :rest :return :screech :sleep-talk :snore :supersonic :surf :swagger :swift :tackle :take-down :toxic :whirlpool :withdraw]
  :cloyster [:hyper-beam :spike-cannon :spikes])

(deflearnset :rs
  :shellder [:attract :aurora-beam :barrier :blizzard :bubblebeam :clamp :dive :double-edge :double-team :endure :explosion :facade :frustration :hail :hidden-power :ice-beam :icicle-spear :icy-wind :leer :mimic :protect :rain-dance :rapid-spin :rest :return :screech :secret-power :selfdestruct :sleep-talk :snore :substitute :supersonic :surf :swagger :swift :tackle :take-down :toxic :water-pulse :withdraw :double-edge :icy-wind :mimic :refresh :selfdestruct]
  :cloyster [:hyper-beam :spike-cannon :spikes :torment])

(deflearnset :dp
  :shellder [:attract :aurora-beam :barrier :blizzard :brine :bubblebeam :captivate :clamp :dive :double-team :endure :explosion :facade :frustration :hail :hidden-power :ice-beam :ice-shard :icicle-spear :icy-wind :iron-defense :leer :mud-shot :natural-gift :payback :protect :rain-dance :rapid-spin :rest :return :rock-blast :screech :secret-power :sleep-talk :snore :substitute :supersonic :surf :swagger :swift :tackle :take-down :toxic :water-pulse :whirlpool :withdraw :double-edge :icy-wind :mimic :refresh :selfdestruct]
  :cloyster [:avalanche :giga-impact :hyper-beam :poison-jab :signal-beam :spike-cannon :spikes :torment :toxic-spikes])

(deflearnset :bw
  :shellder [:aqua-ring :attract :aurora-beam :avalanche :barrier :blizzard :brine :bubblebeam :clamp :dive :double-team :explosion :facade :frustration :hail :hidden-power :hydro-pump :ice-beam :ice-shard :icicle-spear :icy-wind :iron-defense :leer :mud-shot :payback :protect :rain-dance :rapid-spin :razor-shell :rest :return :rock-blast :round :screech :shell-smash :sleep-talk :snore :substitute :supersonic :surf :swagger :tackle :take-down :toxic :twineedle :water-pulse :whirlpool :withdraw :double-edge :icy-wind :mimic :refresh :selfdestruct]
  :cloyster [:frost-breath :giga-impact :hyper-beam :icicle-crash :poison-jab :signal-beam :spike-cannon :spikes :torment :toxic-spikes])
