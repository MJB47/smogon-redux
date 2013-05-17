
(in-ns 'smogon.dex)


(defpokemon :porygon
  :name "Porygon"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:mineral]]
  :abilities [[:trace]
              :bw [:analytic :download :trace]]
  :stats [[65 60 70 75 40]
          :gs [65 60 70 85 75 40]]
  :weight [36.5]
  :height [0.8])

(defpokemon :porygon2
  :name "Porygon2"
  :introduced-in :gs
  :types [[:normal]]
  :egggroups [[:mineral]]
  :abilities [[:trace]
              :dp [:download :trace]
              :bw [:analytic :download :trace]]
  :stats [[85 80 90 105 95 60]]
  :weight [32.5]
  :height [0.6])

(defpokemon :porygon-z
  :name "Porygon-Z"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:mineral]]
  :abilities [[:download :trace]
              :bw [:adaptability :analytic :download]]
  :stats [[85 80 70 135 75 90]]
  :weight [34]
  :height [0.9])



(deffamily (familychain :porygon :porygon2 :porygon-z))

(deflearnset :rb
  :porygon [:agility :bide :blizzard :conversion :double-edge :double-team :flash :hyper-beam :ice-beam :mimic :psybeam :psychic :psywave :rage :recover :reflect :rest :sharpen :skull-bash :substitute :swift :tackle :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic :tri-attack])

(deflearnset :gs
  :porygon [:agility :blizzard :conversion :conversion-2 :curse :double-team :dream-eater :endure :flash :frustration :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :lock-on :nightmare :protect :psybeam :psych-up :psychic :rain-dance :recover :rest :return :sharpen :sleep-talk :snore :sunny-day :swagger :swift :tackle :thief :thunder :thunderbolt :toxic :tri-attack :zap-cannon]
  :porygon2 [:defense-curl])

(deflearnset :rs
  :porygon [:aerial-ace :agility :blizzard :conversion :conversion-2 :double-edge :double-team :dream-eater :endure :facade :flash :frustration :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :lock-on :mimic :nightmare :protect :psybeam :psych-up :psychic :rain-dance :recover :recycle :rest :return :secret-power :shadow-ball :sharpen :shock-wave :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :tackle :thief :thunder :thunder-wave :thunderbolt :toxic :tri-attack :zap-cannon :double-edge :icy-wind :mimic :nightmare]
  :porygon2 [:defense-curl :double-edge :icy-wind :mimic :nightmare])

(deflearnset :dp
  :porygon [:aerial-ace :agility :blizzard :charge-beam :conversion :conversion-2 :discharge :double-team :dream-eater :endure :facade :flash :frustration :giga-impact :gravity :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :last-resort :lock-on :magic-coat :magnet-rise :natural-gift :pain-split :protect :psybeam :psych-up :psychic :rain-dance :recover :recycle :rest :return :secret-power :shadow-ball :sharpen :shock-wave :signal-beam :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :tackle :thief :thunder :thunder-wave :thunderbolt :toxic :tri-attack :trick :trick-room :zap-cannon :zen-headbutt :double-edge :icy-wind :mimic :nightmare]
  :porygon2 [:defense-curl :double-edge :icy-wind :mimic :nightmare]
  :porygon-z [:dark-pulse :embargo :nasty-plot :uproar])

(deflearnset :bw
  :porygon [:aerial-ace :agility :blizzard :charge-beam :conversion :conversion-2 :discharge :double-team :dream-eater :electroweb :facade :flash :foul-play :frustration :giga-impact :gravity :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :last-resort :lock-on :magic-coat :magnet-rise :pain-split :protect :psybeam :psych-up :psychic :psyshock :rain-dance :recover :recycle :rest :return :round :shadow-ball :sharpen :signal-beam :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :tackle :thief :thunder :thunder-wave :thunderbolt :toxic :tri-attack :trick :trick-room :wonder-room :zap-cannon :zen-headbutt :double-edge :icy-wind :mimic :nightmare]
  :porygon2 [:defense-curl :double-edge :icy-wind :mimic :nightmare]
  :porygon-z [:dark-pulse :embargo :nasty-plot :uproar])
