
(in-ns 'smogon.dex)

(defpokemon :dratini
  :name "Dratini"
  :introduced-in :rb
  :types [[:dragon]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:shed-skin]
              :bw [:marvel-scale :shed-skin]]
  :stats [[41 64 45 50 50]
          :gs [41 64 45 50 50 50]]
  :weight [3.3]
  :height [1.8])

(defpokemon :dragonair
  :name "Dragonair"
  :introduced-in :rb
  :types [[:dragon]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:shed-skin]
              :bw [:marvel-scale :shed-skin]]
  :stats [[61 84 65 70 70]
          :gs [61 84 65 70 70 70]]
  :weight [16.5]
  :height [4])

(defpokemon :dragonite
  :name "Dragonite"
  :introduced-in :rb
  :types [[:dragon :flying]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:inner-focus]
              :bw [:inner-focus :multiscale]]
  :stats [[91 134 95 100 80]
          :gs [91 134 95 100 100 80]]
  :weight [210]
  :height [2.2])

(deffamily (familychain :dratini :dragonair :dragonite))

(deflearnset :rb
  :dratini [:agility :bide :blizzard :body-slam :bubblebeam :double-edge :double-team :dragon-rage :fire-blast :hyper-beam :ice-beam :leer :mimic :rage :reflect :rest :skull-bash :slam :substitute :surf :swift :take-down :thunder :thunder-wave :thunderbolt :toxic :water-gun :wrap]
  :dragonair [:horn-drill]
  :dragonite [:razor-wind :strength])

(deflearnset :gs
  :dratini [:agility :attract :blizzard :curse :detect :double-team :dragon-rage :dragonbreath :endure :fire-blast :flamethrower :frustration :haze :headbutt :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :leer :light-screen :mist :outrage :protect :rain-dance :rest :return :safeguard :slam :sleep-talk :snore :supersonic :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :twister :waterfall :wrap :zap-cannon]
  :dragonite [:dynamicpunch :fire-punch :fly :fury-cutter :ice-punch :mud-slap :rock-smash :sandstorm :steel-wing :strength :thunderpunch :whirlpool :wing-attack])

(deflearnset :rs
  :dratini [:agility :attract :blizzard :body-slam :double-edge :double-team :dragon-dance :dragon-rage :dragonbreath :endure :facade :fire-blast :flamethrower :frustration :hail :haze :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :leer :light-screen :mimic :mist :outrage :protect :rain-dance :rest :return :safeguard :secret-power :shock-wave :slam :sleep-talk :snore :substitute :sunny-day :supersonic :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :twister :water-pulse :waterfall :wrap]
  :dragonite [:aerial-ace :brick-break :cut :dive :dragon-claw :dynamicpunch :earthquake :fire-punch :fly :focus-punch :fury-cutter :ice-punch :mud-slap :roar :rock-smash :rock-tomb :sandstorm :steel-wing :strength :thunderpunch :wing-attack])

(deflearnset :dp
  :dratini [:agility :aqua-tail :attract :blizzard :body-slam :captivate :double-edge :double-team :draco-meteor :dragon-dance :dragon-pulse :dragon-rage :dragon-rush :dragonbreath :endure :extremespeed :facade :fire-blast :flamethrower :frustration :hail :haze :headbutt :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :leer :light-screen :mimic :mist :natural-gift :outrage :protect :rain-dance :rest :return :safeguard :secret-power :shock-wave :slam :sleep-talk :snore :substitute :sunny-day :supersonic :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :twister :water-pulse :waterfall :whirlpool :wrap]
  :dragonite [:aerial-ace :air-cutter :brick-break :cut :defog :dive :dragon-claw :earthquake :fire-punch :fling :fly :focus-blast :focus-punch :fury-cutter :giga-impact :heat-wave :ice-punch :iron-head :mud-slap :ominous-wind :roar :rock-slide :rock-smash :rock-tomb :roost :sandstorm :steel-wing :stone-edge :strength :superpower :tailwind :thunderpunch :wing-attack])

(deflearnset :bw
  :dratini [:agility :aqua-jet :aqua-tail :attract :bind :blizzard :body-slam :double-edge :double-team :draco-meteor :dragon-dance :dragon-pulse :dragon-rage :dragon-rush :dragon-tail :dragonbreath :extremespeed :facade :fire-blast :flamethrower :frustration :hail :haze :hidden-power :hyper-beam :ice-beam :icy-wind :incinerate :iron-tail :leer :light-screen :mimic :mist :outrage :protect :rain-dance :rest :return :round :safeguard :slam :shock-wave :sleep-talk :snore :substitute :sunny-day :supersonic :surf :swagger :thunder :thunder-wave :thunderbolt :toxic :twister :water-pulse :waterfall :wrap]
  :dragonite [:aerial-ace :brick-break :bulldoze :cut :dive :dragon-claw :earthquake :fire-punch :fling :fly :focus-blast :giga-impact :heat-wave :hone-claws :hurricane :ice-punch :iron-head :roar :rock-slide :rock-smash :rock-tomb :roost :sandstorm :sky-drop :stone-edge :strength :superpower :tailwind :thunderpunch :wing-attack])
