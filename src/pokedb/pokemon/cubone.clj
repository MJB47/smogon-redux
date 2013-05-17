
(in-ns 'smogon.dex)

(defpokemon :cubone
  :name "Cubone"
  :introduced-in :rb
  :types [[:ground]]
  :egggroups [[:monster]]
  :abilities [[:lightningrod :rock-head]
              :bw [:battle-armor :lightningrod :rock-head]]
  :stats [[50 50 95 40 35]
          :gs [50 50 95 40 50 35]]
  :weight [6.5]
  :height [0.4])

(defpokemon :marowak
  :name "Marowak"
  :introduced-in :rb
  :types [[:ground]]
  :egggroups [[:monster]]
  :abilities [[:lightningrod :rock-head]
              :bw [:battle-armor :lightningrod :rock-head]]
  :stats [[60 80 110 50 45]
          :gs [60 80 110 50 80 45]]
  :weight [45]
  :height [1])

(deffamily (familychain :cubone :marowak))

(deflearnset :rb
  :cubone [:bide :blizzard :body-slam :bone-club :bonemerang :bubblebeam :counter :dig :double-edge :double-team :earthquake :fire-blast :fissure :focus-energy :growl :headbutt :ice-beam :leer :mega-kick :mega-punch :mimic :rage :rest :seismic-toss :skull-bash :strength :submission :substitute :tail-whip :take-down :thrash :toxic :water-gun]
  :marowak [:hyper-beam])

(deflearnset :gs
  :cubone [:ancientpower :attract :belly-drum :blizzard :bone-club :bone-rush :bonemerang :curse :detect :dig :double-team :dynamicpunch :earthquake :endure :false-swipe :fire-blast :fire-punch :flamethrower :focus-energy :frustration :growl :headbutt :hidden-power :ice-beam :icy-wind :iron-tail :leer :mud-slap :perish-song :protect :rage :rest :return :rock-slide :rock-smash :sandstorm :screech :skull-bash :sleep-talk :snore :strength :sunny-day :swagger :swords-dance :tail-whip :thief :thrash :thunderpunch :toxic]
  :marowak [:hyper-beam])

(deflearnset :rs
  :cubone [:aerial-ace :ancientpower :attract :belly-drum :blizzard :body-slam :bone-club :bone-rush :bonemerang :brick-break :counter :dig :double-edge :double-team :dynamicpunch :earthquake :endure :facade :false-swipe :fire-blast :fire-punch :flamethrower :focus-energy :focus-punch :frustration :growl :headbutt :hidden-power :ice-beam :icy-wind :iron-tail :leer :mega-kick :mega-punch :mimic :mud-slap :perish-song :protect :rage :rest :return :rock-slide :rock-smash :rock-tomb :sandstorm :screech :secret-power :seismic-toss :skull-bash :sleep-talk :snore :strength :substitute :sunny-day :swagger :swords-dance :tail-whip :thief :thrash :thunderpunch :toxic]
  :marowak [:hyper-beam :sing :body-slam :icy-wind :mimic :seismic-toss :sing])

(deflearnset :dp
  :cubone [:aerial-ace :ancientpower :attract :belly-drum :blizzard :body-slam :bone-club :bone-rush :bonemerang :brick-break :captivate :detect :dig :double-edge :double-kick :double-team :earth-power :earthquake :endeavor :endure :facade :false-swipe :fire-blast :fire-punch :flamethrower :fling :focus-energy :focus-punch :frustration :fury-cutter :growl :headbutt :hidden-power :ice-beam :icy-wind :iron-defense :iron-head :iron-tail :knock-off :leer :low-kick :mimic :mud-slap :natural-gift :perish-song :protect :rage :rest :return :rock-climb :rock-slide :rock-smash :rock-tomb :sandstorm :screech :secret-power :seismic-toss :skull-bash :sleep-talk :snore :stealth-rock :strength :substitute :sunny-day :swagger :swords-dance :tail-whip :thief :thrash :thunderpunch :toxic :uproar]
  :marowak [:focus-blast :giga-impact :hyper-beam :outrage :sing :stone-edge :body-slam :icy-wind :mimic :seismic-toss :sing])

(deflearnset :bw
  :cubone [:aerial-ace :ancientpower :attract :belly-drum :blizzard :bone-club :bone-rush :bonemerang :brick-break :bulldoze :chip-away :detect :dig :double-edge :double-kick :double-team :earth-power :earthquake :echoed-voice :endeavor :endure :facade :false-swipe :fire-blast :fire-punch :flamethrower :fling :focus-energy :frustration :growl :headbutt :hidden-power :ice-beam :icy-wind :incinerate :iron-defense :iron-head :iron-tail :knock-off :leer :low-kick :mimic :perish-song :protect :rage :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :round :sandstorm :screech :seismic-toss :skull-bash :sleep-talk :smack-down :snore :stealth-rock :strength :substitute :sunny-day :swagger :swords-dance :tail-whip :thief :thrash :thunderpunch :toxic :uproar]
  :marowak [:focus-blast :giga-impact :hyper-beam :outrage :sing :stone-edge :body-slam :icy-wind :mimic :seismic-toss :sing])
