
(in-ns 'smogon.dex)

(defpokemon :raikou
  :name "Raikou"
  :introduced-in :gs
  :types [[:electric]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:pressure :volt-absorb]]
  :stats [[90 85 75 115 100 115]]
  :weight [178]
  :height [1.9])

(deffamily (familychain :raikou))

(deflearnset :gs
  :raikou [:bite :crunch :curse :cut :detect :dig :double-team :endure :flash :frustration :headbutt :hidden-power :hyper-beam :iron-tail :leer :mud-slap :protect :psych-up :quick-attack :rain-dance :reflect :rest :return :roar :rock-smash :sandstorm :sleep-talk :snore :spark :strength :sunny-day :swagger :swift :thunder :thunderbolt :thundershock :toxic :zap-cannon])

(deflearnset :rs
  :raikou [:bite :body-slam :calm-mind :crunch :cut :dig :double-edge :double-team :endure :facade :flash :frustration :hidden-power :hyper-beam :iron-tail :leer :mimic :mud-slap :protect :psych-up :quick-attack :rain-dance :reflect :rest :return :roar :rock-smash :sandstorm :secret-power :shock-wave :sleep-talk :snore :spark :strength :substitute :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :thundershock :toxic :body-slam :double-edge :mimic])

(deflearnset :dp
  :raikou [:bite :calm-mind :charge-beam :crunch :cut :dig :discharge :double-team :endure :extrasensory :facade :flash :frustration :giga-impact :headbutt :hidden-power :hyper-beam :iron-head :iron-tail :leer :light-screen :magnet-rise :mud-slap :natural-gift :protect :psych-up :quick-attack :rain-dance :reflect :rest :return :roar :rock-climb :rock-smash :sandstorm :secret-power :shadow-ball :shock-wave :signal-beam :sleep-talk :snore :spark :strength :substitute :sunny-day :swagger :swift :thunder :thunder-fang :thunder-wave :thunderbolt :thundershock :toxic :zap-cannon :aura-sphere :extremespeed :weather-ball :body-slam :double-edge :mimic])

(deflearnset :bw
  :raikou [:bite :bulldoze :calm-mind :charge-beam :crunch :cut :dig :discharge :double-team :extrasensory :facade :flash :frustration :giga-impact :hidden-power :hyper-beam :iron-head :iron-tail :leer :light-screen :magnet-rise :protect :psych-up :quash :quick-attack :rain-dance :reflect :rest :return :roar :rock-smash :round :sandstorm :shadow-ball :signal-beam :sleep-talk :snarl :snore :spark :strength :substitute :sunny-day :swagger :thunder :thunder-fang :thunder-wave :thunderbolt :thundershock :toxic :volt-switch :wild-charge :zap-cannon :aura-sphere :extremespeed :weather-ball :body-slam :double-edge :mimic])
