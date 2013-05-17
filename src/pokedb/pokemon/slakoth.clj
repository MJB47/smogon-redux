
(in-ns 'smogon.dex)


(defpokemon :slakoth
  :name "Slakoth"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:truant]]
  :stats [[60 60 60 35 35 30]]
  :weight [24]
  :height [0.8])

(defpokemon :vigoroth
  :name "Vigoroth"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:vital-spirit]
              :bw [:vital-spirit]]
  :stats [[80 80 80 55 55 90]]
  :weight [46.5]
  :height [1.4])

(defpokemon :slaking
  :name "Slaking"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:truant]]
  :stats [[150 160 100 95 65 100]]
  :weight [130.5]
  :height [2])

(deffamily (familychain :slakoth :vigoroth :slaking))

(deflearnset :rs
  :slakoth [:aerial-ace :amnesia :attract :blizzard :body-slam :brick-break :bulk-up :counter :covet :crush-claw :curse :cut :double-edge :double-team :dynamicpunch :encore :endure :facade :faint-attack :fire-blast :fire-punch :flail :flamethrower :focus-punch :frustration :fury-cutter :hidden-power :ice-beam :ice-punch :icy-wind :mega-kick :mega-punch :mimic :mud-slap :protect :pursuit :rain-dance :rest :return :rock-slide :rock-smash :scratch :secret-power :seismic-toss :shadow-ball :shock-wave :slack-off :slash :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :thunder :thunderbolt :thunderpunch :toxic :water-pulse :yawn :double-edge :icy-wind :mimic :seismic-toss]
  :vigoroth [:earthquake :focus-energy :fury-swipes :reversal :roar :taunt :uproar :double-edge :icy-wind :mimic :seismic-toss]
  :slaking [:hyper-beam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :slakoth [:aerial-ace :amnesia :attract :blizzard :body-slam :brick-break :bulk-up :captivate :counter :covet :crush-claw :curse :cut :double-team :encore :endure :facade :faint-attack :fire-blast :fire-punch :flail :flamethrower :fling :focus-punch :frustration :fury-cutter :gunk-shot :hammer-arm :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :mud-slap :natural-gift :night-slash :protect :pursuit :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :scratch :secret-power :shadow-ball :shadow-claw :shock-wave :slack-off :slash :sleep-talk :snore :solarbeam :strength :substitute :sucker-punch :sunny-day :swagger :thunder :thunderbolt :thunderpunch :toxic :water-pulse :yawn :double-edge :icy-wind :mimic :seismic-toss]
  :vigoroth [:earthquake :focus-blast :focus-energy :fury-swipes :low-kick :reversal :roar :rock-climb :taunt :uproar :double-edge :icy-wind :mimic :seismic-toss]
  :slaking [:block :giga-impact :hyper-beam :punishment :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :slakoth [:aerial-ace :after-you :amnesia :attract :blizzard :body-slam :brick-break :bulk-up :chip-away :counter :covet :crush-claw :curse :cut :double-team :encore :facade :faint-attack :fire-blast :fire-punch :flail :flamethrower :fling :frustration :gunk-shot :hammer-arm :hidden-power :hone-claws :ice-beam :ice-punch :icy-wind :incinerate :night-slash :protect :pursuit :rain-dance :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :round :scratch :shadow-ball :shadow-claw :slack-off :slash :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :thunder :thunderbolt :thunderpunch :tickle :toxic :work-up :yawn :double-edge :icy-wind :mimic :seismic-toss]
  :vigoroth [:bulldoze :earthquake :endure :focus-blast :focus-energy :focus-punch :fury-swipes :low-kick :low-sweep :reversal :roar :taunt :uproar :double-edge :icy-wind :mimic :seismic-toss]
  :slaking [:block :giga-impact :hyper-beam :punishment :quash :smack-down :double-edge :icy-wind :mimic :seismic-toss])
