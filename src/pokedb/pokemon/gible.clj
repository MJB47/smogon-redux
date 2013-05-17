
(in-ns 'smogon.dex)


(defpokemon :gible
  :name "Gible"
  :introduced-in :dp
  :types [[:dragon :ground]]
  :egggroups [[:dragon :monster]]
  :abilities [[:rough-skin :sand-veil]]
  :stats [[58 70 45 40 45 42]]
  :weight [20.5]
  :height [0.7])

(defpokemon :gabite
  :name "Gabite"
  :introduced-in :dp
  :types [[:dragon :ground]]
  :egggroups [[:dragon :monster]]
  :abilities [[:rough-skin :sand-veil]]
  :stats [[68 90 65 50 55 82]]
  :weight [56]
  :height [1.4])

(defpokemon :garchomp
  :name "Garchomp"
  :introduced-in :dp
  :types [[:dragon :ground]]
  :egggroups [[:dragon :monster]]
  :abilities [[:rough-skin :sand-veil]]
  :stats [[108 130 95 80 85 102]]
  :weight [95]
  :height [1.9])

(deffamily (familychain :gible :gabite :garchomp))

(deflearnset :dp
  :gible [:aerial-ace :attract :body-slam :captivate :cut :dig :double-edge :double-team :draco-meteor :dragon-claw :dragon-pulse :dragon-rage :dragon-rush :dragonbreath :earth-power :earthquake :endure :facade :fire-blast :flamethrower :frustration :fury-cutter :headbutt :hidden-power :iron-head :metal-claw :mud-shot :mud-slap :natural-gift :outrage :protect :rain-dance :rest :return :roar :rock-climb :rock-slide :rock-smash :rock-tomb :sand-attack :sand-tomb :sandstorm :scary-face :secret-power :shadow-claw :slash :sleep-talk :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :swift :tackle :take-down :thrash :toxic :twister]
  :gabite [:iron-tail]
  :garchomp [:aqua-tail :brick-break :crunch :false-swipe :fire-fang :fling :giga-impact :hyper-beam :poison-jab :surf :swords-dance :whirlpool])

(deflearnset :bw
  :gible [:aerial-ace :attract :body-slam :bulldoze :cut :dig :double-edge :double-team :draco-meteor :dragon-claw :dragon-pulse :dragon-rage :dragon-rush :dragonbreath :earth-power :earthquake :facade :fire-blast :flamethrower :frustration :hidden-power :hone-claws :incinerate :iron-head :iron-tail :metal-claw :mud-shot :outrage :protect :rain-dance :rest :return :roar :rock-climb :rock-slide :rock-smash :rock-tomb :round :sand-attack :sand-tomb :sandstorm :scary-face :shadow-claw :slash :sleep-talk :snore :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :tackle :take-down :thrash :toxic :twister]
  :gabite [:dual-chop]
  :garchomp [:aqua-tail :brick-break :crunch :dragon-tail :false-swipe :fire-fang :fling :giga-impact :hyper-beam :poison-jab :surf :swords-dance])
