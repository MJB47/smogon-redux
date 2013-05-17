
(in-ns 'smogon.dex)


(defpokemon :lickitung
  :name "Lickitung"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:monster]]
  :abilities [[:oblivious :own-tempo]
              :bw [:cloud-nine :oblivious :own-tempo]]
  :stats [[90 55 75 60 30]
          :gs [90 55 75 60 75 30]]
  :weight [65.5]
  :height [1.2])

(defpokemon :lickilicky
  :name "Lickilicky"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:monster]]
  :abilities [[:cloud-nine :oblivious :own-tempo]]
  :stats [[110 85 95 80 95 50]]
  :weight [140]
  :height [1.7])


(deffamily (familychain :lickitung :lickilicky))

(deflearnset :rb
  :lickitung [:bide :blizzard :body-slam :bubblebeam :counter :cut :defense-curl :disable :double-edge :double-team :earthquake :fire-blast :fissure :hyper-beam :ice-beam :mega-kick :mega-punch :mimic :rage :rest :screech :seismic-toss :skull-bash :slam :stomp :strength :submission :substitute :supersonic :surf :swords-dance :take-down :thunder :thunderbolt :toxic :water-gun :wrap])

(deflearnset :gs
  :lickitung [:attract :belly-drum :blizzard :body-slam :curse :cut :defense-curl :disable :double-team :dream-eater :dynamicpunch :earthquake :endure :fire-blast :fire-punch :flamethrower :frustration :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :lick :magnitude :mud-slap :nightmare :protect :psych-up :rain-dance :rest :return :rock-smash :rollout :sandstorm :screech :shadow-ball :slam :sleep-talk :snore :stomp :strength :sunny-day :supersonic :surf :swagger :thief :thunder :thunderbolt :thunderpunch :toxic :wrap])

(deflearnset :rs
  :lickitung [:attract :belly-drum :blizzard :body-slam :brick-break :counter :curse :cut :defense-curl :dig :disable :double-edge :double-team :dream-eater :dynamicpunch :earthquake :endure :facade :fire-blast :fire-punch :flamethrower :focus-punch :frustration :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :knock-off :lick :magnitude :mega-kick :mega-punch :mimic :mud-slap :nightmare :protect :psych-up :rain-dance :refresh :rest :return :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :screech :secret-power :seismic-toss :shadow-ball :shock-wave :slam :sleep-talk :smellingsalt :snore :solarbeam :stomp :strength :substitute :sunny-day :supersonic :surf :swagger :swords-dance :thief :thunder :thunderbolt :thunderpunch :toxic :water-pulse :wrap :double-edge :double-edge :helping-hand :icy-wind :mimic :nightmare :seismic-toss])

(deflearnset :dp
  :lickitung [:amnesia :aqua-tail :attract :belly-drum :blizzard :body-slam :brick-break :captivate :curse :cut :defense-curl :dig :disable :double-team :dream-eater :earthquake :endure :facade :fire-blast :fire-punch :flamethrower :fling :focus-punch :frustration :giga-impact :hammer-arm :headbutt :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :iron-tail :knock-off :lick :magnitude :me-first :mud-slap :muddy-water :natural-gift :power-whip :protect :psych-up :rain-dance :refresh :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :rollout :sandstorm :screech :secret-power :shadow-ball :shock-wave :slam :sleep-talk :smellingsalt :snore :solarbeam :stomp :strength :substitute :sunny-day :supersonic :surf :swagger :swords-dance :thief :thunder :thunderbolt :thunderpunch :toxic :water-pulse :whirlpool :wrap :wring-out :zen-headbutt :double-edge :double-edge :helping-hand :icy-wind :mimic :nightmare :seismic-toss]
  :lickilicky [:block :explosion :focus-blast :gyro-ball])

(deflearnset :bw
  :lickitung [:amnesia :aqua-tail :attract :belly-drum :bind :blizzard :body-slam :brick-break :bulldoze :chip-away :curse :cut :defense-curl :dig :disable :double-team :dragon-tail :dream-eater :earthquake :facade :fire-blast :fire-punch :flamethrower :fling :frustration :giga-impact :hammer-arm :hidden-power :hyper-beam :ice-beam :ice-punch :icy-wind :incinerate :iron-tail :knock-off :lick :magnitude :me-first :muddy-water :power-whip :protect :psych-up :rain-dance :refresh :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :rollout :round :sandstorm :screech :shadow-ball :slam :sleep-talk :smellingsalt :snore :solarbeam :stomp :strength :substitute :sunny-day :supersonic :surf :swagger :swords-dance :thief :thunder :thunderbolt :thunderpunch :toxic :work-up :wrap :wring-out :zen-headbutt :double-edge :double-edge :helping-hand :icy-wind :mimic :nightmare :seismic-toss]
  :lickilicky [:block :explosion :focus-blast :gyro-ball])
