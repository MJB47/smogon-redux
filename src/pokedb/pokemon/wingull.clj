
(in-ns 'smogon.dex)


(defpokemon :wingull
  :name "Wingull"
  :introduced-in :rs
  :types [[:flying :water]]
  :egggroups [[:flying :water-1]]
  :abilities [[:keen-eye]
              :bw [:keen-eye :rain-dish]]
  :stats [[40 30 30 55 30 85]]
  :weight [9.5]
  :height [0.6])

(defpokemon :pelipper
  :name "Pelipper"
  :introduced-in :rs
  :types [[:flying :water]]
  :egggroups [[:flying :water-1]]
  :abilities [[:keen-eye]
              :bw [:keen-eye :rain-dish]]
  :stats [[60 50 100 85 70 65]]
  :weight [28]
  :height [1.2])

(deffamily (familychain :wingull :pelipper))



(deflearnset :rs
  :wingull [:aerial-ace :agility :attract :blizzard :double-edge :double-team :endure :facade :fly :frustration :growl :gust :hail :hidden-power :ice-beam :icy-wind :mimic :mist :mud-slap :protect :pursuit :quick-attack :rain-dance :rest :return :secret-power :shock-wave :sky-attack :sleep-talk :snore :steel-wing :substitute :supersonic :swagger :swift :thief :toxic :twister :water-gun :water-pulse :water-sport :wing-attack :double-edge :icy-wind :mimic :sky-attack]
  :pelipper [:hydro-pump :hyper-beam :spit-up :stockpile :surf :swallow :double-edge :icy-wind :mimic :sky-attack])

(deflearnset :dp
  :wingull [:aerial-ace :agility :air-cutter :air-slash :aqua-ring :attract :blizzard :brine :captivate :defog :double-team :endure :facade :fly :frustration :growl :gust :hail :hidden-power :ice-beam :icy-wind :knock-off :mist :mud-slap :natural-gift :ominous-wind :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :roost :secret-power :shock-wave :sleep-talk :snore :steel-wing :substitute :supersonic :swagger :swift :tailwind :thief :toxic :twister :u-turn :uproar :water-gun :water-pulse :water-sport :wing-attack :double-edge :icy-wind :mimic :sky-attack]
  :pelipper [:fling :giga-impact :gunk-shot :hydro-pump :hyper-beam :payback :seed-bomb :sky-attack :spit-up :stockpile :surf :swallow :whirlpool :double-edge :icy-wind :mimic :sky-attack])

(deflearnset :bw
  :wingull [:aerial-ace :agility :air-cutter :air-slash :aqua-ring :attract :blizzard :brine :double-team :echoed-voice :facade :fly :frustration :growl :gust :hail :hidden-power :hurricane :ice-beam :icy-wind :knock-off :mist :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :roost :round :scald :sleep-talk :snore :substitute :supersonic :swagger :tailwind :thief :toxic :twister :u-turn :uproar :water-gun :water-pulse :water-sport :wing-attack :double-edge :icy-wind :mimic :sky-attack]
  :pelipper [:fling :giga-impact :gunk-shot :hydro-pump :hyper-beam :payback :seed-bomb :sky-attack :sky-drop :soak :spit-up :stockpile :surf :swallow :double-edge :icy-wind :mimic :sky-attack])

