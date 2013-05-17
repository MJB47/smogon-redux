
(in-ns 'smogon.dex)

(defpokemon :jirachi
  :name "Jirachi"
  :introduced-in :rs
  :types [[:psychic :steel]]
  :egggroups [[]]
  :abilities [[:serene-grace]
              :bw [:serene-grace]]
  :stats [[100 100 100 100 100 100]]
  :weight [1.1]
  :height [0.3])

(deffamily (familychain :jirachi))

(deflearnset :rs
  :jirachi [:aerial-ace :body-slam :calm-mind :confusion :cosmic-power :defense-curl :doom-desire :double-edge :double-team :dream-eater :dynamicpunch :endure :facade :fire-punch :flash :frustration :future-sight :helping-hand :hidden-power :hyper-beam :ice-punch :icy-wind :light-screen :metronome :mimic :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :reflect :refresh :rest :return :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snore :substitute :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :water-pulse :wish :body-slam :icy-wind :mimic :nightmare :thunder-wave])

(deflearnset :dp
  :jirachi [:aerial-ace :ancientpower :calm-mind :charge-beam :confusion :cosmic-power :doom-desire :double-edge :double-team :drain-punch :dream-eater :endure :energy-ball :facade :fire-punch :flash :flash-cannon :fling :frustration :future-sight :giga-impact :grass-knot :gravity :headbutt :healing-wish :helping-hand :hidden-power :hyper-beam :ice-punch :icy-wind :iron-defense :iron-head :last-resort :light-screen :magic-coat :mud-slap :natural-gift :protect :psych-up :psychic :rain-dance :recycle :reflect :refresh :rest :return :safeguard :sandstorm :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snore :stealth-rock :substitute :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :u-turn :uproar :water-pulse :wish :zen-headbutt :draco-meteor :body-slam :icy-wind :mimic :nightmare :thunder-wave])

(deflearnset :bw
  :jirachi [:aerial-ace :calm-mind :charge-beam :confusion :cosmic-power :doom-desire :double-edge :double-team :drain-punch :dream-eater :energy-ball :facade :fire-punch :flash :flash-cannon :fling :frustration :future-sight :giga-impact :grass-knot :gravity :healing-wish :helping-hand :hidden-power :hyper-beam :ice-punch :icy-wind :iron-defense :iron-head :last-resort :light-screen :magic-coat :magic-room :protect :psych-up :psychic :psyshock :rain-dance :recycle :reflect :refresh :rest :return :round :safeguard :sandstorm :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :stealth-rock :substitute :sunny-day :swagger :swift :telekinesis :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :u-turn :uproar :wish :zen-headbutt :draco-meteor :body-slam :icy-wind :mimic :nightmare :thunder-wave])
