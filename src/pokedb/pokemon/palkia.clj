
(in-ns 'smogon.dex)


(defpokemon :palkia
  :name "Palkia"
  :introduced-in :dp
  :types [[:dragon :water]]
  :egggroups [[]]
  :abilities [[:pressure :telepathy]]
  :stats [[90 120 100 150 120 100]]
  :weight [336]
  :height [4.2])

(deffamily (familychain :palkia))

(deflearnset :dp
  :palkia [:aerial-ace :ancientpower :aqua-tail :aura-sphere :avalanche :blizzard :brick-break :brine :bulk-up :cut :dive :double-team :draco-meteor :dragon-claw :dragon-pulse :dragonbreath :earth-power :earthquake :endure :facade :fire-blast :flamethrower :fling :focus-blast :focus-punch :frustration :fury-cutter :giga-impact :gravity :hail :headbutt :heal-block :hidden-power :hydro-pump :hyper-beam :ice-beam :mud-slap :natural-gift :outrage :power-gem :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :safeguard :sandstorm :scary-face :secret-power :shadow-claw :shock-wave :slash :sleep-talk :snore :spacial-rend :stone-edge :strength :substitute :sunny-day :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :trick-room :twister :water-pulse :whirlpool])

(deflearnset :bw
  :palkia [:aerial-ace :ancientpower :aqua-tail :aura-sphere :blizzard :brick-break :bulk-up :bulldoze :cut :dive :double-team :draco-meteor :dragon-claw :dragon-pulse :dragon-tail :dragonbreath :earth-power :earthquake :echoed-voice :facade :fire-blast :flamethrower :fling :focus-blast :frustration :giga-impact :gravity :hail :hidden-power :hone-claws :hydro-pump :hyper-beam :hyper-voice :ice-beam :incinerate :outrage :power-gem :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :round :safeguard :sandstorm :scary-face :shadow-claw :slash :sleep-talk :snore :spacial-rend :stone-edge :strength :substitute :sunny-day :surf :swagger :thunder :thunder-wave :thunderbolt :toxic :trick-room :water-pulse])
