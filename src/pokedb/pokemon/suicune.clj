
(in-ns 'smogon.dex)

(defpokemon :suicune
  :name "Suicune"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure :water-absorb]]
  :stats [[100 75 115 90 115 85]]
  :weight [187]
  :height [2])

(deffamily (familychain :suicune))

(deflearnset :gs
  :suicune [:aurora-beam :bite :blizzard :bubblebeam :curse :cut :detect :dig :double-team :endure :frustration :gust :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-tail :leer :mirror-coat :mist :mud-slap :protect :psych-up :rain-dance :rest :return :roar :rock-smash :sandstorm :sleep-talk :snore :sunny-day :surf :swagger :swift :toxic :water-gun :waterfall :whirlpool])

(deflearnset :rs
  :suicune [:aurora-beam :bite :blizzard :body-slam :bubblebeam :calm-mind :cut :dig :dive :double-edge :double-team :endure :facade :frustration :gust :hail :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-tail :leer :mimic :mirror-coat :mist :mud-slap :protect :psych-up :rain-dance :reflect :rest :return :roar :rock-smash :sandstorm :secret-power :sleep-talk :snore :substitute :sunny-day :surf :swagger :swift :toxic :water-pulse :waterfall :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :suicune [:aurora-beam :avalanche :bite :blizzard :brine :bubblebeam :calm-mind :cut :dig :dive :double-team :endure :extrasensory :facade :frustration :giga-impact :gust :hail :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :ice-fang :icy-wind :iron-head :iron-tail :leer :mirror-coat :mist :mud-slap :natural-gift :ominous-wind :protect :psych-up :rain-dance :reflect :rest :return :roar :rock-climb :rock-smash :sandstorm :secret-power :shadow-ball :signal-beam :sleep-talk :snore :substitute :sunny-day :surf :swagger :swift :tailwind :toxic :water-pulse :waterfall :whirlpool :sheer-cold :air-slash :aqua-ring :extremespeed :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :suicune [:aurora-beam :bite :blizzard :bubblebeam :bulldoze :calm-mind :cut :dig :dive :double-team :extrasensory :facade :frustration :giga-impact :gust :hail :hidden-power :hydro-pump :hyper-beam :ice-beam :ice-fang :icy-wind :iron-head :iron-tail :leer :mirror-coat :mist :protect :psych-up :quash :rain-dance :reflect :rest :return :roar :rock-smash :round :sandstorm :scald :shadow-ball :signal-beam :sleep-talk :snarl :snore :substitute :sunny-day :surf :swagger :tailwind :toxic :waterfall :sheer-cold :air-slash :aqua-ring :extremespeed :body-slam :double-edge :icy-wind :mimic])

