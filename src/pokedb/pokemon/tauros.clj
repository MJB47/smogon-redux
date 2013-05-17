
(in-ns 'smogon.dex)


(defpokemon :tauros
  :name "Tauros"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:intimidate]
              :dp [:anger-point :intimidate]
              :bw [:anger-point :intimidate :sheer-force]]
  :stats [[75 100 95 70 110]
          :gs [75 100 95 40 70 110]]
  :weight [88.4]
  :height [1.4])

(deffamily (familychain :tauros))

(deflearnset :rb
  :tauros [:bide :blizzard :body-slam :double-edge :double-team :earthquake :fire-blast :fissure :horn-drill :hyper-beam :ice-beam :leer :mimic :rage :rest :skull-bash :stomp :strength :substitute :tackle :tail-whip :take-down :thunder :thunderbolt :toxic])

(deflearnset :gs
  :tauros [:attract :blizzard :curse :double-team :earthquake :endure :fire-blast :flamethrower :frustration :headbutt :hidden-power :horn-attack :hyper-beam :ice-beam :icy-wind :iron-tail :protect :pursuit :rage :rest :return :rock-smash :scary-face :sleep-talk :snore :strength :sunny-day :surf :swagger :tackle :tail-whip :take-down :thrash :thunder :thunderbolt :toxic :zap-cannon])

(deflearnset :rs
  :tauros [:attract :blizzard :body-slam :double-edge :double-team :earthquake :endure :facade :fire-blast :flamethrower :frustration :hidden-power :horn-attack :hyper-beam :ice-beam :icy-wind :iron-tail :mimic :protect :pursuit :rage :rain-dance :rest :return :rock-smash :rock-tomb :sandstorm :scary-face :secret-power :shock-wave :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :surf :swagger :tackle :tail-whip :take-down :thrash :thunder :thunderbolt :toxic :water-pulse :body-slam :body-slam :double-edge :icy-wind :mimic :refresh])

(deflearnset :dp
  :tauros [:attract :blizzard :captivate :double-team :earthquake :endeavor :endure :facade :fire-blast :flamethrower :frustration :giga-impact :headbutt :helping-hand :hidden-power :horn-attack :hyper-beam :ice-beam :icy-wind :iron-head :iron-tail :natural-gift :outrage :payback :protect :pursuit :rage :rain-dance :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :role-play :sandstorm :scary-face :secret-power :shock-wave :sleep-talk :snore :solarbeam :spite :stone-edge :strength :substitute :sunny-day :surf :swagger :tackle :tail-whip :take-down :thrash :thunder :thunderbolt :toxic :uproar :water-pulse :whirlpool :zen-headbutt :body-slam :body-slam :double-edge :icy-wind :mimic :refresh])

(deflearnset :bw
  :tauros [:attract :blizzard :bulldoze :double-team :earthquake :endeavor :facade :fire-blast :flamethrower :frustration :giga-impact :helping-hand :hidden-power :horn-attack :hyper-beam :ice-beam :icy-wind :incinerate :iron-head :iron-tail :outrage :payback :protect :pursuit :rage :rain-dance :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :role-play :round :sandstorm :scary-face :sleep-talk :snore :solarbeam :spite :stone-edge :strength :substitute :sunny-day :surf :swagger :tackle :tail-whip :take-down :thrash :thunder :thunderbolt :toxic :uproar :wild-charge :work-up :zen-headbutt :body-slam :body-slam :double-edge :icy-wind :mimic :refresh])
