
(in-ns 'smogon.dex)


(defpokemon :girafarig
  :name "Girafarig"
  :introduced-in :gs
  :types [[:normal :psychic]]
  :egggroups [[:ground]]
  :abilities [[:early-bird :inner-focus]
              :bw [:early-bird :inner-focus :sap-sipper]]
  :stats [[70 80 65 90 65 85]]
  :weight [41.5]
  :height [1.5])

(deffamily (familychain :girafarig))

(deflearnset :gs
  :girafarig [:agility :amnesia :attract :baton-pass :beat-up :confusion :crunch :curse :double-team :dream-eater :earthquake :endure :foresight :frustration :future-sight :growl :headbutt :hidden-power :iron-tail :mud-slap :nightmare :protect :psybeam :psych-up :psychic :rest :return :rock-smash :shadow-ball :sleep-talk :snore :stomp :strength :sunny-day :swagger :swift :tackle :take-down :thief :thunder :thunderbolt :toxic :zap-cannon])

(deflearnset :rs
  :girafarig [:agility :amnesia :astonish :attract :baton-pass :beat-up :body-slam :calm-mind :confusion :crunch :double-edge :double-team :dream-eater :earthquake :endure :facade :flash :foresight :frustration :future-sight :growl :hidden-power :iron-tail :light-screen :magic-coat :mimic :mud-slap :nightmare :odor-sleuth :protect :psybeam :psych-up :psychic :rain-dance :reflect :rest :return :rock-smash :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snore :stomp :strength :substitute :sunny-day :swagger :swift :tackle :take-down :thief :thunder :thunder-wave :thunderbolt :toxic :wish :body-slam :double-edge :mimic :nightmare :swagger])

(deflearnset :dp
  :girafarig [:agility :amnesia :assurance :astonish :attract :baton-pass :beat-up :calm-mind :captivate :charge-beam :confusion :crunch :double-hit :double-kick :double-team :dream-eater :earthquake :endure :energy-ball :facade :flash :foresight :frustration :future-sight :grass-knot :gravity :growl :guard-swap :headbutt :hidden-power :iron-tail :light-screen :magic-coat :mirror-coat :mud-slap :natural-gift :odor-sleuth :power-swap :protect :psybeam :psych-up :psychic :rain-dance :razor-wind :recycle :reflect :rest :return :rock-smash :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snore :stomp :strength :substitute :sucker-punch :sunny-day :swagger :swift :tackle :take-down :thief :thunder :thunder-wave :thunderbolt :toxic :trick :trick-room :uproar :wish :zen-headbutt :body-slam :double-edge :mimic :nightmare :swagger])

(deflearnset :bw
  :girafarig [:agility :amnesia :assurance :astonish :attract :baton-pass :beat-up :bulldoze :calm-mind :charge-beam :confusion :crunch :double-hit :double-kick :double-team :dream-eater :earthquake :echoed-voice :energy-ball :facade :flash :foresight :foul-play :frustration :future-sight :grass-knot :gravity :growl :guard-swap :hidden-power :hyper-voice :iron-tail :light-screen :magic-coat :mean-look :mirror-coat :odor-sleuth :power-swap :protect :psybeam :psych-up :psychic :psyshock :rain-dance :razor-wind :recycle :reflect :rest :retaliate :return :rock-smash :round :secret-power :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :stomp :strength :substitute :sunny-day :swagger :tackle :take-down :telekinesis :thief :thunder :thunder-wave :thunderbolt :toxic :trick :trick-room :uproar :wish :work-up :zen-headbutt :body-slam :double-edge :mimic :nightmare :swagger])
