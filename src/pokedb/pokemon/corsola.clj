
(in-ns 'smogon.dex)

(defpokemon :corsola
  :name "Corsola"
  :introduced-in :gs
  :types [[:rock :water]]
  :egggroups [[:water-1 :water-3]]
  :abilities [[:hustle :natural-cure]
              :bw [:hustle :natural-cure :regenerator]]
  :stats [[55 55 85 65 85 35]]
  :weight [5]
  :height [0.6])

(deffamily (familychain :corsola))

(deflearnset :gs
  :corsola [:amnesia :ancientpower :attract :bubble :bubblebeam :curse :defense-curl :double-team :earthquake :endure :frustration :harden :headbutt :hidden-power :ice-beam :mirror-coat :mist :mud-slap :protect :psychic :rain-dance :recover :rest :return :rock-slide :rock-smash :rollout :safeguard :sandstorm :screech :sleep-talk :snore :spike-cannon :strength :sunny-day :surf :swagger :tackle :toxic :whirlpool])

(deflearnset :rs
  :corsola [:amnesia :ancientpower :attract :barrier :blizzard :body-slam :bubble :bubblebeam :calm-mind :confuse-ray :defense-curl :dig :double-edge :double-team :earthquake :endure :explosion :facade :frustration :hail :harden :hidden-power :ice-beam :icicle-spear :ingrain :light-screen :mimic :mirror-coat :mist :mud-slap :protect :psychic :rain-dance :recover :reflect :refresh :rest :return :rock-blast :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sandstorm :screech :secret-power :selfdestruct :shadow-ball :sleep-talk :snore :spike-cannon :strength :substitute :sunny-day :surf :swagger :tackle :toxic :water-pulse])

(deflearnset :dp
  :corsola [:amnesia :ancientpower :aqua-ring :attract :barrier :blizzard :body-slam :brine :bubble :bubblebeam :calm-mind :captivate :confuse-ray :curse :dig :double-edge :double-team :earth-power :earthquake :endeavor :endure :explosion :facade :frustration :hail :harden :headbutt :hidden-power :ice-beam :icicle-spear :icy-wind :ingrain :light-screen :lucky-chant :magic-coat :mimic :mirror-coat :mist :mud-slap :natural-gift :nature-power :power-gem :protect :psychic :rain-dance :recover :reflect :refresh :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sandstorm :screech :secret-power :selfdestruct :shadow-ball :sleep-talk :snore :spike-cannon :stealth-rock :stone-edge :strength :substitute :sucker-punch :sunny-day :surf :swagger :tackle :toxic :water-pulse :whirlpool])

(deflearnset :bw
  :corsola [:amnesia :ancientpower :aqua-ring :attract :barrier :bide :blizzard :body-slam :bubble :bubblebeam :bulldoze :calm-mind :confuse-ray :curse :dig :double-edge :double-team :earth-power :earthquake :endeavor :endure :explosion :facade :flail :frustration :hail :harden :head-smash :hidden-power :ice-beam :icicle-spear :icy-wind :ingrain :iron-defense :light-screen :lucky-chant :magic-coat :mimic :mirror-coat :mist :nature-power :power-gem :protect :psychic :rain-dance :recover :reflect :refresh :rest :return :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :round :safeguard :sandstorm :scald :screech :selfdestruct :shadow-ball :sleep-talk :snore :spike-cannon :stealth-rock :stone-edge :strength :substitute :sunny-day :surf :swagger :tackle :toxic :water-pulse])
