
(in-ns 'smogon.dex)


(defpokemon :bronzor
  :name "Bronzor"
  :introduced-in :dp
  :types [[:psychic :steel]]
  :egggroups [[:mineral]]
  :abilities [[:heatproof :heavy-metal :levitate]]
  :stats [[57 24 86 24 86 23]]
  :weight [60.5]
  :height [0.5])

(defpokemon :bronzong
  :name "Bronzong"
  :introduced-in :dp
  :types [[:psychic :steel]]
  :egggroups [[:mineral]]
  :abilities [[:heatproof :heavy-metal :levitate]]
  :stats [[67 89 116 79 116 33]]
  :weight [187]
  :height [1.3])

(deffamily (familychain :bronzor :bronzong))

(deflearnset :dp
  :bronzor [:ancientpower :calm-mind :charge-beam :confuse-ray :confusion :double-team :dream-eater :earthquake :endure :extrasensory :facade :faint-attack :flash :flash-cannon :frustration :future-sight :grass-knot :gravity :gyro-ball :heal-block :hidden-power :hypnosis :imprison :iron-defense :light-screen :natural-gift :payback :protect :psych-up :psychic :rain-dance :recycle :reflect :rest :return :rock-polish :rock-slide :rock-tomb :rollout :safeguard :sandstorm :secret-power :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :solarbeam :stealth-rock :substitute :sunny-day :swagger :tackle :toxic :trick :trick-room]
  :bronzong [:block :explosion :giga-impact :hyper-beam :iron-head :rock-smash :strength :zen-headbutt])

(deflearnset :bw
  :bronzor [:bulldoze :calm-mind :charge-beam :confuse-ray :confusion :double-team :dream-eater :earthquake :extrasensory :facade :faint-attack :flash :flash-cannon :frustration :future-sight :grass-knot :gravity :gyro-ball :heal-block :heavy-slam :hidden-power :hypnosis :imprison :iron-defense :light-screen :metal-sound :payback :protect :psych-up :psychic :psyshock :psywave :rain-dance :recycle :reflect :rest :return :rock-polish :rock-slide :rock-tomb :round :safeguard :sandstorm :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :solarbeam :stealth-rock :substitute :sunny-day :swagger :tackle :telekinesis :toxic :trick :trick-room :wonder-room]
  :bronzong [:block :explosion :giga-impact :hyper-beam :iron-head :rock-smash :strength :zen-headbutt])
