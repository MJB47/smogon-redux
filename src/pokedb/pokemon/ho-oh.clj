
(in-ns 'smogon.dex)


(defpokemon :ho-oh
  :name "Ho-Oh"
  :introduced-in :gs
  :types [[:fire :flying]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure :regenerator]]
  :stats [[106 130 90 110 154 90]]
  :weight [199]
  :height [3.8])

(deffamily (familychain :ho-oh))

(deflearnset :gs
  :ho-oh [:ancientpower :curse :detect :double-team :dragonbreath :dream-eater :earthquake :endure :fire-blast :flamethrower :flash :fly :frustration :future-sight :giga-drain :gust :hidden-power :hyper-beam :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :recover :rest :return :roar :rock-smash :sacred-fire :safeguard :sandstorm :shadow-ball :sleep-talk :snore :solarbeam :steel-wing :strength :sunny-day :swagger :swift :thunder :thunderbolt :toxic :whirlwind :zap-cannon])

(deflearnset :rs
  :ho-oh [:aerial-ace :ancientpower :calm-mind :double-edge :double-team :dream-eater :earthquake :endure :facade :fire-blast :flamethrower :flash :fly :frustration :future-sight :giga-drain :gust :hidden-power :hyper-beam :light-screen :mimic :mud-slap :nightmare :overheat :protect :psych-up :psychic :rain-dance :recover :reflect :rest :return :roar :rock-smash :sacred-fire :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :sky-attack :sleep-talk :snore :solarbeam :steel-wing :strength :substitute :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :whirlwind :double-edge :mimic :nightmare])

(deflearnset :dp
  :ho-oh [:aerial-ace :air-cutter :ancientpower :brave-bird :calm-mind :charge-beam :defog :double-team :dream-eater :earth-power :earthquake :endure :extrasensory :facade :fire-blast :flamethrower :flash :fly :frustration :future-sight :giga-drain :giga-impact :gust :heat-wave :hidden-power :hyper-beam :iron-head :light-screen :mud-slap :natural-gift :ominous-wind :overheat :pluck :protect :psych-up :psychic :punishment :rain-dance :recover :reflect :rest :return :roar :rock-smash :roost :sacred-fire :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :signal-beam :sky-attack :sleep-talk :snore :solarbeam :steel-wing :strength :substitute :sunny-day :swagger :swift :tailwind :thunder :thunder-wave :thunderbolt :toxic :twister :weather-ball :whirlwind :will-o-wisp :zen-headbutt :double-edge :mimic :nightmare])

(deflearnset :bw
  :ho-oh [:aerial-ace :ancientpower :brave-bird :bulldoze :calm-mind :charge-beam :double-team :dream-eater :earth-power :earthquake :echoed-voice :extrasensory :facade :fire-blast :flame-charge :flamethrower :flash :fly :frustration :future-sight :giga-drain :giga-impact :gust :heat-wave :hidden-power :hyper-beam :hyper-voice :incinerate :iron-head :light-screen :natural-gift :overheat :pluck :protect :psych-up :psychic :punishment :rain-dance :recover :reflect :rest :return :roar :rock-smash :roost :round :sacred-fire :safeguard :sandstorm :shadow-ball :signal-beam :sky-attack :sky-drop :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :tailwind :thunder :thunder-wave :thunderbolt :toxic :weather-ball :whirlwind :will-o-wisp :zen-headbutt :double-edge :mimic :nightmare])
