
(in-ns 'smogon.dex)

(defpokemon :delibird
  :name "Delibird"
  :introduced-in :gs   
  :types [[:flying :ice]]
  :egggroups [[:ground :water-1]]
  :abilities [[:hustle :vital-spirit]
              :bw [:hustle :insomnia :vital-spirit]]
  :stats [[45 55 45 65 45 75]]
  :weight [16]
  :height [0.9])

(deffamily (familychain :delibird))

(deflearnset :gs
  :delibird [:attract :aurora-beam :blizzard :curse :detect :double-team :endure :fly :frustration :future-sight :headbutt :hidden-power :ice-beam :icy-wind :mud-slap :present :protect :quick-attack :rain-dance :rapid-spin :rest :return :sleep-talk :snore :splash :swagger :swift :thief :toxic])

(deflearnset :rs
  :delibird [:aerial-ace :attract :aurora-beam :blizzard :body-slam :counter :double-edge :double-team :endure :facade :fly :focus-punch :frustration :future-sight :hail :hidden-power :ice-ball :ice-beam :icy-wind :mega-kick :mega-punch :mimic :mud-slap :present :protect :quick-attack :rain-dance :rapid-spin :rest :return :secret-power :seismic-toss :sky-attack :sleep-talk :snore :splash :substitute :swagger :swift :thief :toxic :water-pulse])

(deflearnset :dp
  :delibird [:aerial-ace :attract :aurora-beam :avalanche :blizzard :body-slam :bounce :brick-break :captivate :defog :double-edge :double-team :endure :facade :fake-out :fling :fly :focus-punch :frustration :future-sight :gunk-shot :hail :headbutt :hidden-power :ice-ball :ice-beam :ice-punch :ice-shard :icy-wind :mimic :mud-slap :natural-gift :pluck :present :protect :quick-attack :rain-dance :rapid-spin :recycle :rest :return :rollout :secret-power :seed-bomb :seismic-toss :signal-beam :sky-attack :sleep-talk :splash :substitute :swagger :swift :thief :toxic :water-pulse])

(deflearnset :bw
  :delibird [:aerial-ace :attract :aurora-beam :bestow :blizzard :body-slam :bounce :brick-break :double-edge :double-team :facade :fake-out :fling :fly :frost-breath :frustration :future-sight :gunk-shot :hail :hidden-power :ice-ball :ice-beam :ice-punch :ice-shard :icy-wind :mimic :pluck :present :protect :quick-attack :rain-dance :rapid-spin :recycle :rest :return :round :seed-bomb :seismic-toss :signal-beam :sky-attack :sleep-talk :splash :substitute :swagger :thief :toxic])
