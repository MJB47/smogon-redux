
(in-ns 'smogon.dex)


(defpokemon :sandshrew
  :name "Sandshrew"
  :introduced-in :rb
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:sand-veil]
              :bw [:sand-rush :sand-veil]]
  :stats [[50 75 85 30 40]
          :gs [50 75 85 20 30 40]]
  :weight [12]
  :height [0.6])

(defpokemon :sandslash
  :name "Sandslash"
  :introduced-in :rb
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:sand-veil]
              :bw [:sand-rush :sand-veil]]
  :stats [[75 100 110 55 65]
          :gs [75 100 110 45 55 65]]
  :weight [29.5]
  :height [1])

(deffamily (familychain :sandshrew :sandslash))

(deflearnset :rb
  :sandshrew [:bide :body-slam :cut :dig :double-edge :double-team :earthquake :fissure :fury-swipes :mimic :poison-sting :rage :rest :rock-slide :sand-attack :scratch :seismic-toss :skull-bash :slash :strength :submission :substitute :swift :swords-dance :take-down :toxic]
  :sandslash [:hyper-beam])

(deflearnset :gs
  :sandshrew [:attract :counter :curse :cut :defense-curl :detect :dig :double-team :dynamicpunch :earthquake :endure :flail :frustration :fury-cutter :fury-swipes :headbutt :hidden-power :iron-tail :metal-claw :mud-slap :poison-sting :protect :rapid-spin :rest :return :rock-smash :rollout :safeguard :sand-attack :sandstorm :scratch :slash :sleep-talk :snore :strength :sunny-day :swagger :swift :thief :toxic]
  :sandslash [:hyper-beam])

(deflearnset :rs
  :sandshrew [:aerial-ace :attract :body-slam :brick-break :counter :crush-claw :cut :defense-curl :dig :double-edge :double-team :dynamicpunch :earthquake :endure :facade :flail :focus-punch :frustration :fury-cutter :fury-swipes :hidden-power :iron-tail :metal-claw :mimic :mud-slap :poison-sting :protect :rapid-spin :rest :return :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sand-attack :sand-tomb :sandstorm :scratch :secret-power :seismic-toss :slash :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :thief :toxic :body-slam :double-edge :mimic :seismic-toss]
  :sandslash [:hyper-beam :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :sandshrew [:aerial-ace :attract :brick-break :captivate :counter :crush-claw :cut :defense-curl :dig :double-team :earth-power :earthquake :endure :facade :flail :fling :focus-punch :frustration :fury-cutter :fury-swipes :gyro-ball :headbutt :hidden-power :iron-tail :knock-off :metal-claw :mud-shot :mud-slap :natural-gift :night-slash :poison-jab :poison-sting :protect :rapid-spin :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sand-attack :sand-tomb :sandstorm :scratch :secret-power :shadow-claw :slash :sleep-talk :snore :stealth-rock :strength :substitute :sunny-day :super-fang :swagger :swift :swords-dance :thief :toxic :x-scissor :body-slam :double-edge :mimic :seismic-toss]
  :sandslash [:focus-blast :giga-impact :hyper-beam :stone-edge :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :sandshrew [:aerial-ace :attract :brick-break :bulldoze :chip-away :counter :covet :crush-claw :cut :defense-curl :dig :double-team :earth-power :earthquake :endure :facade :flail :fling :frustration :fury-cutter :fury-swipes :gyro-ball :hidden-power :hone-claws :iron-tail :knock-off :magnitude :metal-claw :mud-shot :night-slash :poison-jab :poison-sting :protect :rapid-spin :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :rollout :round :safeguard :sand-attack :sand-tomb :sandstorm :scratch :shadow-claw :slash :sleep-talk :snore :stealth-rock :strength :substitute :sunny-day :super-fang :swagger :swift :swords-dance :thief :toxic :x-scissor :body-slam :double-edge :mimic :seismic-toss]
  :sandslash [:focus-blast :giga-impact :hyper-beam :stone-edge :body-slam :double-edge :mimic :seismic-toss])
