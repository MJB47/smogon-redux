
(in-ns 'smogon.dex)

(defpokemon :chingling
  :name "Chingling"
  :introduced-in :dp
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:levitate]]
  :stats [[45 30 50 65 50 45]]
  :weight [0.6]
  :height [0.2])

(defpokemon :chimecho
  :name "Chimecho"
  :introduced-in :rs
  :types [[:psychic]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[65 50 70 95 80 65]]
  :weight [1]
  :height [0.6])

(deffamily (familychain :chingling :chimecho))

(deflearnset :rs
  :chimecho [:astonish :attract :calm-mind :confusion :curse :defense-curl :disable :double-edge :double-team :dream-eater :endure :facade :flash :frustration :growl :heal-bell :hidden-power :hypnosis :icy-wind :light-screen :mimic :nightmare :protect :psych-up :psychic :psywave :rain-dance :reflect :rest :return :rollout :safeguard :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :take-down :taunt :torment :toxic :uproar :wrap :yawn])

(deflearnset :dp
  :chingling [:astonish :attract :calm-mind :captivate :charge-beam :confusion :curse :disable :double-team :dream-eater :endure :facade :flash :frustration :future-sight :grass-knot :gravity :growl :heal-bell :helping-hand :hidden-power :hypnosis :icy-wind :knock-off :last-resort :light-screen :magic-coat :mimic :natural-gift :nightmare :protect :psych-up :psychic :rain-dance :recover :recycle :reflect :rest :return :rollout :safeguard :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snatch :snore :substitute :sunny-day :swagger :swift :taunt :thunder-wave :torment :toxic :trick :trick-room :uproar :wish :wrap :zen-headbutt]
  :chimecho [:double-edge :energy-ball :extrasensory :healing-wish :psywave :take-down :yawn])

(deflearnset :bw
  :chingling [:astonish :attract :bind :calm-mind :charge-beam :confusion :curse :disable :double-team :dream-eater :echoed-voice :entrainment :facade :flash :frustration :future-sight :grass-knot :gravity :growl :heal-bell :helping-hand :hidden-power :hyper-voice :hypnosis :icy-wind :knock-off :last-resort :light-screen :magic-coat :mimic :nightmare :protect :psych-up :psychic :psyshock :rain-dance :recover :recycle :reflect :rest :return :round :safeguard :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :stored-power :substitute :sunny-day :swagger :taunt :telekinesis :thunder-wave :torment :toxic :trick :trick-room :uproar :wish :wrap :zen-headbutt]
  :chimecho [:double-edge :energy-ball :extrasensory :heal-pulse :healing-wish :psywave :synchronoise :take-down :yawn])
