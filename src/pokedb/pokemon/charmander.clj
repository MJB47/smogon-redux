
(in-ns 'smogon.dex)

(defpokemon :charmander
  :name "Charmander"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:dragon :monster]]
  :abilities [[:blaze]
              :bw [:blaze :solar-power]]
  :stats [[39 52 43 50 65]
          :gs [39 52 43 60 50 65]]
  :weight [8.5]
  :height [0.6])

(defpokemon :charmeleon
  :name "Charmeleon"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:dragon :monster]]
  :abilities [[:blaze]
              :bw [:blaze :solar-power]]
  :stats [[58 64 58 65 80]
          :gs [58 64 58 80 65 80]]
  :weight [19]
  :height [1.1])

(defpokemon :charizard
  :name "Charizard"
  :introduced-in :rb
  :types [[:fire :flying]]
  :egggroups [[:dragon :monster]]
  :abilities [[:blaze]
              :bw [:blaze :solar-power]]
  :stats [[78 84 78 85 100]
          :gs [78 84 78 109 85 100]]
  :weight [90.5]
  :height [1.7])

(deffamily (familychain :charmander :charmeleon :charizard))

(deflearnset :rb
  :charmander [:bide :body-slam :counter :cut :dig :double-edge :double-team :dragon-rage :ember :fire-blast :fire-spin :flamethrower :growl :leer :mega-kick :mega-punch :mimic :rage :reflect :rest :scratch :seismic-toss :skull-bash :slash :strength :submission :substitute :swift :swords-dance :take-down :toxic :crunch]
  :charizard [:earthquake :fissure :fly :hyper-beam])

(deflearnset :gs
  :charmander [:ancientpower :attract :beat-up :belly-drum :bite :curse :cut :defense-curl :dig :double-team :dragon-rage :dragonbreath :dynamicpunch :ember :endure :fire-blast :fire-punch :fire-spin :flamethrower :frustration :fury-cutter :growl :headbutt :hidden-power :iron-tail :mud-slap :outrage :protect :rage :rest :return :rock-slide :rock-smash :scary-face :scratch :slash :sleep-talk :smokescreen :snore :strength :sunny-day :swagger :swift :toxic :crunch]
  :charizard [:earthquake :fly :hyper-beam :roar :sandstorm :steel-wing :wing-attack])

(deflearnset :rs
  :charmander [:aerial-ace :ancientpower :attract :beat-up :belly-drum :bite :body-slam :brick-break :counter :cut :defense-curl :dig :double-edge :double-team :dragon-claw :dragon-dance :dragon-rage :dynamicpunch :ember :endure :facade :fire-blast :fire-punch :fire-spin :flamethrower :focus-punch :frustration :fury-cutter :growl :hidden-power :iron-tail :mega-kick :mega-punch :metal-claw :mimic :mud-slap :outrage :overheat :protect :rage :rest :return :rock-slide :rock-smash :scary-face :scratch :secret-power :seismic-toss :slash :sleep-talk :smokescreen :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :toxic]
  :charizard [:blast-burn :earthquake :fly :heat-wave :hyper-beam :roar :steel-wing :wing-attack])

(deflearnset :dp
  :charmander [:aerial-ace :ancientpower :attract :beat-up :belly-drum :bite :body-slam :brick-break :captivate :counter :crunch :cut :dig :double-edge :double-team :dragon-claw :dragon-dance :dragon-rage :dragon-rush :ember :endure :facade :fire-blast :fire-fang :fire-punch :fire-spin :flamethrower :flare-blitz :fling :focus-punch :frustration :fury-cutter :growl :headbutt :heat-wave :hidden-power :iron-tail :metal-claw :mimic :mud-slap :natural-gift :outrage :overheat :protect :rage :rest :return :rock-slide :rock-smash :rock-tomb :scary-face :scratch :secret-power :seismic-toss :shadow-claw :slash :sleep-talk :smokescreen :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :thunderpunch :toxic :will-o-wisp]
  :charizard [:air-cutter :air-slash :blast-burn :defog :dragon-pulse :earthquake :fly :focus-blast :giga-impact :hyper-beam :ominous-wind :roar :roost :solarbeam :steel-wing :tailwind :twister :wing-attack])

(deflearnset :bw
  :charmander [:aerial-ace :ancientpower :attract :beat-up :belly-drum :bite :body-slam :brick-break :counter :crunch :cut :dig :double-edge :double-team :dragon-claw :dragon-dance :dragon-pulse :dragon-rage :dragon-rush :echoed-voice :ember :facade :fire-blast :fire-fang :fire-pledge :fire-punch :fire-spin :flame-burst :flame-charge :flamethrower :flare-blitz :fling :focus-punch :frustration :growl :heat-wave :hidden-power :hone-claws :incinerate :inferno :iron-tail :metal-claw :mimic :outrage :overheat :protect :rage :rest :return :rock-slide :rock-smash :rock-tomb :round :scary-face :scratch :seismic-toss :shadow-claw :slash :sleep-talk :smokescreen :snore :strength :substitute :sunny-day :swagger :swords-dance :thunderpunch :toxic :will-o-wisp :acrobatics :block :false-swipe]
  :charizard [:air-slash :blast-burn :bulldoze :dragon-tail :earthquake :fly :focus-blast :giga-impact :hyper-beam :roar :roost :sky-drop :solarbeam :tailwind :wing-attack])
