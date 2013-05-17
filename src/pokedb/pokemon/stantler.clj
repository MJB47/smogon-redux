
(in-ns 'smogon.dex)


(defpokemon :stantler
  :name "Stantler"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:intimidate]
              :dp [:frisk :intimidate]
              :bw [:frisk :intimidate :sap-sipper]]
  :stats [[73 95 62 85 65 85]]
  :weight [71.2]
  :height [1.4])

(deffamily (familychain :stantler))

(deflearnset :gs
  :stantler [:attract :bite :confuse-ray :curse :detect :disable :double-team :dream-eater :earthquake :endure :flash :frustration :headbutt :hidden-power :hypnosis :leer :light-screen :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :reflect :rest :return :roar :sand-attack :sleep-talk :snore :spite :stomp :sunny-day :swagger :swift :tackle :take-down :thief :toxic])

(deflearnset :rs
  :stantler [:astonish :attract :bite :body-slam :calm-mind :confuse-ray :disable :double-edge :double-team :dream-eater :earthquake :endure :extrasensory :facade :flash :frustration :hidden-power :hypnosis :iron-tail :leer :light-screen :mimic :mud-slap :nightmare :protect :psych-up :psychic :rain-dance :reflect :rest :return :roar :role-play :sand-attack :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snore :solarbeam :spite :stomp :substitute :sunny-day :swagger :swift :tackle :take-down :thief :thunder :thunder-wave :thunderbolt :toxic :body-slam :double-edge :mimic :nightmare])

(deflearnset :dp
  :stantler [:astonish :attract :bite :bounce :calm-mind :captivate :charge-beam :confuse-ray :disable :double-kick :double-team :dream-eater :earthquake :endure :energy-ball :extrasensory :facade :flash :frustration :giga-impact :gravity :headbutt :hidden-power :hypnosis :imprison :iron-tail :last-resort :leer :light-screen :me-first :megahorn :mud-slap :natural-gift :protect :psych-up :psychic :rain-dance :reflect :rest :return :roar :role-play :sand-attack :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snore :solarbeam :spite :stomp :substitute :sucker-punch :sunny-day :swagger :swift :tackle :take-down :thief :thrash :thunder :thunder-wave :thunderbolt :toxic :trick-room :uproar :zen-headbutt :body-slam :double-edge :mimic :nightmare])

(deflearnset :bw
  :stantler [:astonish :attract :bite :bounce :bulldoze :calm-mind :captivate :charge-beam :confuse-ray :disable :double-kick :double-team :dream-eater :earthquake :energy-ball :extrasensory :facade :flash :frustration :giga-impact :gravity :hidden-power :hypnosis :imprison :iron-tail :jump-kick :last-resort :leer :light-screen :magic-room :me-first :megahorn :mud-sport :protect :psych-up :psychic :psyshock :rage :rain-dance :reflect :rest :retaliate :return :roar :role-play :round :sand-attack :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :solarbeam :spite :stomp :substitute :sunny-day :swagger :tackle :take-down :thief :thrash :thunder :thunder-wave :thunderbolt :toxic :trick-room :uproar :wild-charge :work-up :zen-headbutt :body-slam :double-edge :mimic :nightmare])
