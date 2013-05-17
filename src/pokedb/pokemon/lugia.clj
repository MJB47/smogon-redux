
(in-ns 'smogon.dex)


(defpokemon :lugia
  :name "Lugia"
  :introduced-in :gs
  :types [[:flying :psychic]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:multiscale :pressure]]
  :stats [[106 90 130 90 154 110]]
  :weight [216]
  :height [5.2])

(deffamily (familychain :lugia))

(deflearnset :gs
  :lugia [:aeroblast :ancientpower :blizzard :curse :detect :double-team :dragonbreath :dream-eater :earthquake :endure :fly :frustration :future-sight :giga-drain :gust :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-tail :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :recover :rest :return :roar :rock-smash :safeguard :sandstorm :shadow-ball :sleep-talk :snore :steel-wing :strength :sunny-day :surf :swagger :swift :thunder :thunderbolt :toxic :waterfall :whirlpool :whirlwind :zap-cannon])

(deflearnset :rs
  :lugia [:aerial-ace :aeroblast :ancientpower :blizzard :body-slam :calm-mind :dive :double-edge :double-team :dream-eater :earthquake :endure :facade :fly :frustration :future-sight :giga-drain :gust :hail :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-tail :light-screen :mimic :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :recover :reflect :rest :return :roar :rock-smash :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snore :steel-wing :strength :substitute :sunny-day :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :water-pulse :waterfall :whirlwind :body-slam :double-edge :featherdance :icy-wind :mimic :nightmare :psycho-boost])

(deflearnset :dp
  :lugia [:aerial-ace :aeroblast :air-cutter :ancientpower :aqua-tail :avalanche :blizzard :brine :calm-mind :charge-beam :defog :dive :double-team :dragon-pulse :dragon-rush :dream-eater :earth-power :earthquake :endure :extrasensory :facade :flash :fly :frustration :future-sight :giga-drain :giga-impact :gust :hail :headbutt :hidden-power :hydro-pump :hyper-beam :ice-beam :icy-wind :iron-head :iron-tail :light-screen :mud-slap :natural-gift :ominous-wind :protect :psych-up :psychic :punishment :rain-dance :recover :reflect :rest :return :roar :rock-smash :roost :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sky-attack :sleep-talk :snore :steel-wing :strength :substitute :sunny-day :surf :swagger :swift :tailwind :thunder :thunder-wave :thunderbolt :toxic :trick :twister :water-pulse :waterfall :weather-ball :whirlpool :whirlwind :zen-headbutt :body-slam :double-edge :featherdance :icy-wind :mimic :nightmare :psycho-boost])

(deflearnset :bw
  :lugia [:aerial-ace :aeroblast :ancientpower :aqua-tail :blizzard :bulldoze :calm-mind :charge-beam :dive :double-team :dragon-pulse :dragon-rush :dragon-tail :dream-eater :earth-power :earthquake :echoed-voice :extrasensory :facade :flash :fly :frustration :future-sight :giga-drain :giga-impact :gust :hail :hidden-power :hydro-pump :hyper-beam :hyper-voice :ice-beam :icy-wind :iron-head :iron-tail :light-screen :natural-gift :protect :psych-up :psychic :psyshock :punishment :rain-dance :recover :reflect :rest :return :roar :rock-smash :roost :round :safeguard :sandstorm :shadow-ball :signal-beam :skill-swap :sky-attack :sky-drop :sleep-talk :snore :strength :substitute :sunny-day :surf :swagger :tailwind :telekinesis :thunder :thunder-wave :thunderbolt :toxic :trick :waterfall :weather-ball :whirlwind :wonder-room :zen-headbutt :body-slam :double-edge :featherdance :icy-wind :mimic :nightmare :psycho-boost])
