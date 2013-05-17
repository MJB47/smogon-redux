
(in-ns 'smogon.dex)


(defpokemon :totodile
  :name "Totodile"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:sheer-force :torrent]]
  :stats [[50 65 64 44 48 43]]
  :weight [9.5]
  :height [0.6])

(defpokemon :croconaw
  :name "Croconaw"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:sheer-force :torrent]]
  :stats [[65 80 80 59 63 58]]
  :weight [25]
  :height [1.1])

(defpokemon :feraligatr
  :name "Feraligatr"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:monster :water-1]]
  :abilities [[:torrent]
              :bw [:sheer-force :torrent]]
  :stats [[85 105 100 79 83 78]]
  :weight [88.8]
  :height [2.3])

(deffamily (familychain :totodile :croconaw :feraligatr))



(deflearnset :gs
  :totodile [:ancientpower :attract :bite :blizzard :crunch :curse :cut :detect :dig :double-team :dynamicpunch :endure :frustration :headbutt :hidden-power :hydro-pump :ice-beam :ice-punch :icy-wind :iron-tail :leer :mud-slap :protect :rage :rain-dance :razor-wind :rest :return :rock-slide :scary-face :scratch :screech :slash :sleep-talk :snore :surf :swagger :thrash :toxic :water-gun :whirlpool]
  :croconaw [:fury-cutter :roar :rock-smash :strength]
  :feraligatr [:earthquake :hyper-beam])

(deflearnset :rs
  :totodile [:aerial-ace :ancientpower :attract :bite :blizzard :body-slam :brick-break :counter :crunch :cut :dig :dive :double-edge :double-team :dragon-claw :dynamicpunch :endure :facade :focus-punch :frustration :hail :hidden-power :hydro-pump :ice-beam :ice-punch :icy-wind :iron-tail :leer :mega-kick :mega-punch :mimic :mud-slap :mud-sport :protect :rage :rain-dance :rest :return :rock-slide :scary-face :scratch :screech :secret-power :seismic-toss :slash :sleep-talk :snore :substitute :surf :swagger :swords-dance :thrash :toxic :water-gun :water-pulse :water-sport :waterfall]
  :croconaw [:fury-cutter :roar :rock-smash :strength]
  :feraligatr [:earthquake :hyper-beam])

(deflearnset :dp
  :totodile [:aerial-ace :ancientpower :aqua-jet :aqua-tail :attract :bite :blizzard :brick-break :captivate :crunch :cut :dig :dive :double-team :dragon-claw :dragon-dance :endure :facade :flail :fling :focus-punch :frustration :hail :headbutt :hidden-power :hydro-pump :ice-beam :ice-fang :ice-punch :icy-wind :iron-tail :leer :low-kick :metal-claw :mud-slap :mud-sport :natural-gift :protect :rage :rain-dance :rest :return :rock-slide :rock-tomb :scary-face :scratch :screech :secret-power :shadow-claw :slash :sleep-talk :snore :spite :substitute :superpower :surf :swagger :swords-dance :thrash :toxic :uproar :water-gun :water-pulse :water-sport :waterfall :whirlpool]
  :croconaw [:fury-cutter :roar :rock-smash :strength]
  :feraligatr [:agility :avalanche :dragon-pulse :earthquake :focus-blast :giga-impact :hydro-cannon :hyper-beam :outrage :rock-climb])

(deflearnset :bw
  :totodile [:aerial-ace :ancientpower :aqua-jet :aqua-tail :attract :bite :blizzard :block :brick-break :chip-away :crunch :cut :dig :dive :double-team :dragon-claw :dragon-dance :facade :fake-tears :flail :fling :frustration :hail :hidden-power :hone-claws :hydro-pump :ice-beam :ice-fang :ice-punch :icy-wind :iron-tail :leer :low-kick :metal-claw :mud-sport :protect :rage :rain-dance :rest :return :rock-slide :rock-tomb :round :scald :scary-face :scratch :screech :shadow-claw :slash :sleep-talk :snore :spite :substitute :superpower :surf :swagger :swords-dance :thrash :toxic :uproar :water-gun :water-pledge :water-pulse :water-sport :waterfall]
  :croconaw [:roar :rock-smash :strength]
  :feraligatr [:agility :bulldoze :dragon-pulse :dragon-tail :earthquake :focus-blast :giga-impact :hydro-cannon :hyper-beam :outrage])

