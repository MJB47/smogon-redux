
(in-ns 'smogon.dex)


(defpokemon :shieldon
  :name "Shieldon"
  :introduced-in :dp
  :types [[:rock :steel]]
  :egggroups [[:monster]]
  :abilities [[:soundproof :sturdy]]
  :stats [[30 42 118 42 88 30]]
  :weight [57]
  :height [0.5])

(defpokemon :bastiodon
  :name "Bastiodon"
  :introduced-in :dp
  :types [[:rock :steel]]
  :egggroups [[:monster]]
  :abilities [[:soundproof :sturdy]]
  :stats [[60 52 168 47 138 30]]
  :weight [149.5]
  :height [1.3])

(deffamily (familychain :shieldon :bastiodon))

(deflearnset :dp
  :shieldon [:ancientpower :attract :blizzard :body-slam :captivate :counter :curse :dig :double-edge :double-team :earth-power :earthquake :endure :facade :fire-blast :fissure :flamethrower :flash-cannon :focus-energy :frustration :headbutt :hidden-power :ice-beam :iron-defense :iron-head :iron-tail :magnet-rise :metal-burst :metal-sound :mud-slap :natural-gift :protect :rain-dance :rest :return :roar :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :sandstorm :scary-face :screech :secret-power :shock-wave :sleep-talk :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :tackle :take-down :taunt :thunder :thunderbolt :torment :toxic]
  :bastiodon [:avalanche :block :giga-impact :hyper-beam :magic-coat :outrage])

(deflearnset :bw
  :shieldon [:ancientpower :attract :blizzard :body-slam :bulldoze :counter :curse :dig :double-edge :double-team :earth-power :earthquake :endure :facade :fire-blast :fissure :flamethrower :flash-cannon :focus-energy :frustration :headbutt :heavy-slam :hidden-power :ice-beam :incinerate :iron-defense :iron-head :iron-tail :magnet-rise :metal-burst :metal-sound :protect :rain-dance :rest :return :roar :rock-blast :rock-polish :rock-slide :rock-smash :rock-tomb :round :sandstorm :scary-face :screech :sleep-talk :smack-down :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :tackle :take-down :taunt :thunder :thunderbolt :torment :toxic :wide-guard]
  :bastiodon [:block :giga-impact :hyper-beam :magic-coat :outrage])
