
(in-ns 'smogon.dex)

(defpokemon :entei
  :name "Entei"
  :introduced-in :gs
  :types [[:fire]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:flash-fire :pressure]]
  :stats [[115 115 85 90 75 100]]
  :weight [198]
  :height [2.1])

(deffamily (familychain :entei))

(deflearnset :gs
  :entei [:bite :curse :cut :detect :dig :double-team :ember :endure :fire-blast :fire-spin :flamethrower :flash :frustration :headbutt :hidden-power :hyper-beam :iron-tail :leer :mud-slap :protect :psych-up :rain-dance :rest :return :roar :rock-smash :sandstorm :sleep-talk :snore :solarbeam :stomp :strength :sunny-day :swagger :swift :toxic])

(deflearnset :rs
  :entei [:bite :body-slam :calm-mind :cut :dig :double-edge :double-team :ember :endure :facade :fire-blast :fire-spin :flamethrower :flash :frustration :hidden-power :hyper-beam :iron-tail :leer :mimic :mud-slap :protect :psych-up :rain-dance :reflect :rest :return :roar :rock-smash :sandstorm :secret-power :sleep-talk :snore :solarbeam :stomp :strength :substitute :sunny-day :swagger :swift :toxic :body-slam :double-edge :mimic])

(deflearnset :dp
  :entei [:bite :calm-mind :cut :dig :double-team :ember :endure :eruption :extrasensory :facade :fire-blast :fire-fang :fire-spin :flamethrower :flash :frustration :giga-impact :headbutt :heat-wave :hidden-power :hyper-beam :iron-head :iron-tail :lava-plume :leer :mud-slap :natural-gift :overheat :protect :psych-up :rain-dance :reflect :rest :return :roar :rock-climb :rock-smash :sandstorm :secret-power :shadow-ball :sleep-talk :snore :solarbeam :stomp :stone-edge :strength :substitute :sunny-day :swagger :swift :toxic :will-o-wisp :flare-blitz :howl :extremespeed :crush-claw :body-slam :double-edge :mimic])

(deflearnset :bw
  :entei [:bite :bulldoze :calm-mind :cut :dig :double-team :ember :eruption :extrasensory :facade :fire-blast :fire-fang :fire-spin :flame-charge :flamethrower :flash :frustration :giga-impact :heat-wave :hidden-power :hyper-beam :incinerate :iron-head :iron-tail :lava-plume :leer :overheat :protect :psych-up :quash :rain-dance :reflect :rest :return :roar :rock-smash :round :sandstorm :shadow-ball :sleep-talk :snarl :snore :solarbeam :stomp :stone-edge :strength :substitute :sunny-day :swagger :toxic :will-o-wisp :flare-blitz :howl :extremespeed :crush-claw :body-slam :double-edge :mimic])
