
(in-ns 'smogon.dex)


(defpokemon :treecko
  :name "Treecko"
  :introduced-in :rs
  :types [[:grass]]
  :egggroups [[:dragon :monster]]
  :abilities [[:overgrow]
              :bw [:overgrow :unburden]]
  :stats [[40 45 35 65 55 70]]
  :weight [5]
  :height [0.5])

(defpokemon :grovyle
  :name "Grovyle"
  :introduced-in :rs
  :types [[:grass]]
  :egggroups [[:dragon :monster]]
  :abilities [[:overgrow]
              :bw [:overgrow :unburden]]
  :stats [[50 65 45 85 65 95]]
  :weight [21.6]
  :height [0.9])

(defpokemon :sceptile
  :name "Sceptile"
  :introduced-in :rs
  :types [[:grass]]
  :egggroups [[:dragon :monster]]
  :abilities [[:overgrow]
              :bw [:overgrow :unburden]]
  :stats [[70 85 65 105 85 120]]
  :weight [52.2]
  :height [1.7])

(deffamily (familychain :treecko :grovyle :sceptile))



(deflearnset :rs
  :treecko [:absorb :aerial-ace :agility :attract :body-slam :brick-break :bullet-seed :counter :crunch :crush-claw :cut :detect :dig :double-edge :double-team :dragonbreath :dynamicpunch :endeavor :endure :facade :flash :focus-punch :frustration :fury-cutter :giga-drain :hidden-power :iron-tail :leech-seed :leer :mega-drain :mega-kick :mega-punch :mimic :mud-slap :mud-sport :pound :protect :pursuit :quick-attack :rest :return :rock-smash :rock-tomb :safeguard :screech :secret-power :seismic-toss :slam :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :swift :swords-dance :thunderpunch :toxic :body-slam :double-edge :mimic :seismic-toss]
  :grovyle [:false-swipe :leaf-blade :body-slam :double-edge :mimic :seismic-toss]
  :sceptile [:dragon-claw :earthquake :hyper-beam :roar :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :treecko [:absorb :aerial-ace :agility :attract :brick-break :bullet-seed :captivate :crunch :crush-claw :cut :detect :dig :double-kick :double-team :dragonbreath :drain-punch :endeavor :endure :energy-ball :facade :flash :fling :focus-punch :frustration :fury-cutter :giga-drain :grass-knot :grasswhistle :headbutt :hidden-power :iron-tail :leaf-storm :leech-seed :leer :low-kick :magical-leaf :mega-drain :mud-slap :mud-sport :natural-gift :pound :protect :pursuit :quick-attack :razor-wind :rest :return :rock-slide :rock-smash :rock-tomb :safeguard :screech :secret-power :seed-bomb :slam :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :swift :swords-dance :synthesis :thunderpunch :toxic :worry-seed :body-slam :double-edge :mimic :seismic-toss]
  :grovyle [:false-swipe :leaf-blade :x-scissor :body-slam :double-edge :mimic :seismic-toss]
  :sceptile [:dragon-claw :dragon-pulse :earthquake :focus-blast :frenzy-plant :giga-impact :hyper-beam :night-slash :outrage :roar :rock-climb :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :treecko [:absorb :acrobatics :aerial-ace :agility :attract :brick-break :bullet-seed :crunch :crush-claw :cut :detect :dig :double-kick :double-team :dragonbreath :drain-punch :endeavor :energy-ball :facade :flash :fling :frustration :giga-drain :grass-knot :grass-pledge :grasswhistle :hidden-power :iron-tail :leaf-storm :leech-seed :leer :low-kick :magical-leaf :mega-drain :mud-sport :natural-gift :pound :protect :pursuit :quick-attack :razor-wind :rest :return :rock-slide :rock-smash :rock-tomb :round :safeguard :screech :seed-bomb :slam :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :swagger :swords-dance :synthesis :thunderpunch :toxic :worry-seed :body-slam :double-edge :mimic :seismic-toss]
  :grovyle [:false-swipe :fury-cutter :leaf-blade :low-sweep :x-scissor :body-slam :double-edge :mimic :seismic-toss]
  :sceptile [:bulldoze :dragon-claw :dragon-pulse :earthquake :focus-blast :frenzy-plant :giga-impact :hone-claws :hyper-beam :night-slash :outrage :roar :body-slam :double-edge :mimic :seismic-toss])

