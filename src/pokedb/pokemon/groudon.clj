
(in-ns 'smogon.dex)


(defpokemon :groudon
  :name "Groudon"
  :introduced-in :rs
  :types [[:ground]]
  :egggroups [[]]
  :abilities [[:drought]
              :bw [:drought]]
  :stats [[100 150 140 100 90 90]]
  :weight [950]
  :height [3.5])

(deffamily (familychain :groudon))

(deflearnset :rs
  :groudon [:aerial-ace :ancientpower :body-slam :brick-break :bulk-up :counter :cut :defense-curl :dig :double-edge :double-team :dragon-claw :dynamicpunch :earthquake :endure :eruption :facade :fire-blast :fire-punch :fissure :flamethrower :frustration :fury-cutter :hidden-power :hyper-beam :iron-tail :mega-kick :mega-punch :mimic :mud-shot :mud-slap :overheat :protect :psych-up :rest :return :roar :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sandstorm :scary-face :secret-power :seismic-toss :shock-wave :slash :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :swift :swords-dance :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :groudon [:aerial-ace :ancientpower :block :brick-break :bulk-up :cut :dig :double-team :dragon-claw :dragon-pulse :earth-power :earthquake :endure :eruption :facade :fire-blast :fire-punch :fissure :flamethrower :fling :focus-blast :frustration :fury-cutter :giga-impact :hammer-arm :headbutt :hidden-power :hyper-beam :iron-head :iron-tail :lava-plume :mud-shot :mud-slap :natural-gift :overheat :protect :psych-up :rest :return :roar :rock-climb :rock-polish :rock-slide :rock-smash :rock-tomb :rollout :safeguard :sandstorm :scary-face :secret-power :shadow-claw :shock-wave :slash :sleep-talk :snore :solarbeam :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :swift :swords-dance :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :uproar :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :groudon [:aerial-ace :ancientpower :block :brick-break :bulk-up :bulldoze :cut :dig :double-team :dragon-claw :dragon-pulse :dragon-tail :earth-power :earthquake :eruption :facade :fire-blast :fire-punch :fissure :flamethrower :fling :focus-blast :frustration :giga-impact :hammer-arm :hidden-power :hone-claws :hyper-beam :incinerate :iron-head :iron-tail :lava-plume :mud-shot :overheat :protect :psych-up :rest :return :roar :rock-polish :rock-slide :rock-smash :rock-tomb :round :safeguard :sandstorm :scary-face :shadow-claw :sleep-talk :smack-down :snore :solarbeam :stealth-rock :stone-edge :strength :substitute :sunny-day :swagger :swords-dance :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :uproar :body-slam :double-edge :mimic :seismic-toss])
