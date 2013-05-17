
(in-ns 'smogon.dex)

(defpokemon :rayquaza
  :name "Rayquaza"
  :introduced-in :rs
  :types [[:dragon :flying]]
  :egggroups [[]]
  :abilities [[:air-lock]
              :bw [:air-lock]]
  :stats [[105 150 90 150 90 95]]
  :weight [206.5]
  :height [7])

(deffamily (familychain :rayquaza))

(deflearnset :rs
  :rayquaza [:aerial-ace :ancientpower :blizzard :body-slam :brick-break :bulk-up :crunch :dive :double-edge :double-team :dragon-claw :dragon-dance :earthquake :endure :extremespeed :facade :fire-blast :flamethrower :fly :frustration :fury-cutter :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :mimic :mud-slap :outrage :overheat :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :sandstorm :scary-face :secret-power :shock-wave :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :surf :swagger :swift :thunder :thunder-wave :thunderbolt :toxic :twister :water-pulse :waterfall :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :rayquaza [:aerial-ace :air-slash :ancientpower :aqua-tail :avalanche :blizzard :brick-break :bulk-up :crunch :dive :double-team :draco-meteor :dragon-claw :dragon-dance :dragon-pulse :earth-power :earthquake :endure :energy-ball :extremespeed :facade :fire-blast :flamethrower :fling :fly :focus-blast :frustration :fury-cutter :giga-impact :gyro-ball :headbutt :hidden-power :hyper-beam :hyper-voice :ice-beam :icy-wind :iron-head :iron-tail :mud-slap :natural-gift :outrage :overheat :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :sandstorm :scary-face :secret-power :shadow-claw :shock-wave :sleep-talk :snore :solarbeam :stone-edge :strength :substitute :sunny-day :surf :swagger :swift :swords-dance :tailwind :thunder :thunder-wave :thunderbolt :toxic :twister :uproar :water-pulse :waterfall :whirlpool :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :rayquaza [:aerial-ace :air-slash :ancientpower :aqua-tail :bind :blizzard :brick-break :bulk-up :bulldoze :crunch :dive :double-team :draco-meteor :dragon-claw :dragon-dance :dragon-pulse :dragon-tail :earth-power :earthquake :echoed-voice :energy-ball :extremespeed :facade :fire-blast :flamethrower :fling :fly :focus-blast :frustration :giga-impact :gyro-ball :hidden-power :hone-claws :hyper-beam :hyper-voice :ice-beam :icy-wind :incinerate :iron-head :iron-tail :outrage :overheat :protect :psych-up :rain-dance :rest :return :roar :rock-slide :rock-smash :rock-tomb :round :sandstorm :scary-face :shadow-claw :sky-drop :sleep-talk :snore :solarbeam :stone-edge :strength :substitute :sunny-day :surf :swagger :swords-dance :tailwind :thunder :thunder-wave :thunderbolt :toxic :twister :uproar :v-create :waterfall :body-slam :double-edge :icy-wind :mimic])
