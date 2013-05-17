
(in-ns 'smogon.dex)

(defpokemon :articuno
  :name "Articuno"
  :introduced-in :rb
  :types [[:flying :ice]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure :snow-cloak]]
  :stats [[90 85 100 125 85]
          :gs [90 85 100 95 125 85]]
  :weight [55.4]
  :height [1.7])

(deffamily (familychain :articuno))

(deflearnset :rb
  :articuno [:agility :bide :blizzard :bubblebeam :double-edge :double-team :fly :hyper-beam :ice-beam :mimic :mist :peck :rage :razor-wind :reflect :rest :sky-attack :substitute :swift :take-down :toxic :water-gun :whirlwind])

(deflearnset :gs
  :articuno [:agility :blizzard :curse :detect :double-team :endure :fly :frustration :gust :hidden-power :hyper-beam :ice-beam :icy-wind :mind-reader :mist :mud-slap :powder-snow :protect :rain-dance :reflect :rest :return :roar :rock-smash :sandstorm :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :toxic])

(deflearnset :rs
  :articuno [:aerial-ace :agility :blizzard :double-edge :double-team :endure :facade :fly :frustration :gust :hail :hidden-power :hyper-beam :ice-beam :icy-wind :mimic :mind-reader :mist :mud-slap :powder-snow :protect :rain-dance :reflect :rest :return :roar :rock-smash :sandstorm :secret-power :sheer-cold :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :toxic :water-pulse])

(deflearnset :dp
  :articuno [:aerial-ace :agility :air-cutter :ancientpower :avalanche :blizzard :defog :double-team :endure :facade :fly :frustration :giga-impact :gust :hail :hidden-power :hyper-beam :ice-beam :ice-shard :icy-wind :mind-reader :mist :mud-slap :natural-gift :ominous-wind :pluck :powder-snow :protect :rain-dance :reflect :rest :return :roar :rock-smash :roost :sandstorm :secret-power :sheer-cold :signal-beam :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :tailwind :toxic :twister :u-turn :water-pulse])

(deflearnset :bw
  :articuno [:aerial-ace :agility :ancientpower :blizzard :double-edge :double-team :extrasensory :facade :fly :frost-breath :frustration :giga-impact :gust :hail :haze :heal-bell :hidden-power :hurricane :hyper-beam :ice-beam :ice-shard :icy-wind :mimic :mind-reader :mist :pluck :powder-snow :protect :rain-dance :reflect :rest :return :roar :rock-smash :roost :round :sandstorm :sheer-cold :signal-beam :sky-attack :sky-drop :sleep-talk :snore :substitute :sunny-day :swagger :tailwind :toxic :u-turn])
