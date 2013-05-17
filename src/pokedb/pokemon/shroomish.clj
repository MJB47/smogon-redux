
(in-ns 'smogon.dex)


(defpokemon :shroomish
  :name "Shroomish"
  :introduced-in :rs
  :types [[:grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:effect-spore]
              :dp [:effect-spore :poison-heal]
              :bw [:effect-spore :poison-heal :quick-feet]]
  :stats [[60 40 60 40 60 35]]
  :weight [4.5]
  :height [0.4])

(defpokemon :breloom
  :name "Breloom"
  :introduced-in :rs
  :types [[:fighting :grass]]
  :egggroups [[:fairy :plant]]
  :abilities [[:effect-spore]
              :dp [:effect-spore :poison-heal]
              :bw [:effect-spore :poison-heal :technician]]
  :stats [[60 130 80 60 60 70]]
  :weight [39.2]
  :height [1.2])

(deffamily (familychain :shroomish :breloom))

(deflearnset :rs
  :shroomish [:absorb :attract :body-slam :bullet-seed :charm :double-edge :double-team :endure :facade :fake-tears :false-swipe :flash :frustration :giga-drain :growth :headbutt :helping-hand :hidden-power :leech-seed :mega-drain :mimic :poisonpowder :protect :rest :return :safeguard :secret-power :sleep-talk :sludge-bomb :snatch :snore :solarbeam :spore :stun-spore :substitute :sunny-day :swagger :swords-dance :tackle :toxic :body-slam :double-edge :mimic :refresh]
  :breloom [:brick-break :bulk-up :counter :cut :dynamicpunch :focus-punch :fury-cutter :hyper-beam :iron-tail :mach-punch :mega-kick :mega-punch :mind-reader :mud-slap :rock-smash :seismic-toss :sky-uppercut :strength :thunderpunch])

(deflearnset :dp
  :shroomish [:absorb :attract :bullet-seed :captivate :charm :double-team :endure :energy-ball :facade :fake-tears :false-swipe :flash :frustration :giga-drain :grass-knot :growth :headbutt :helping-hand :hidden-power :leech-seed :mega-drain :natural-gift :poisonpowder :protect :rest :return :safeguard :secret-power :seed-bomb :sleep-talk :sludge-bomb :snatch :snore :solarbeam :spore :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :tackle :toxic :wake-up-slap :worry-seed :body-slam :double-edge :mimic :refresh]
  :breloom [:brick-break :bulk-up :counter :cut :drain-punch :dynamicpunch :fling :focus-blast :focus-punch :force-palm :fury-cutter :giga-impact :hyper-beam :iron-tail :mach-punch :mind-reader :mud-slap :rock-slide :rock-smash :rock-tomb :sky-uppercut :stone-edge :strength :superpower :thunderpunch :vacuum-wave])

(deflearnset :bw
  :shroomish [:absorb :attract :bullet-seed :charm :double-team :drain-punch :energy-ball :facade :fake-tears :false-swipe :flash :focus-punch :frustration :giga-drain :grass-knot :growth :headbutt :helping-hand :hidden-power :leech-seed :mega-drain :natural-gift :poisonpowder :protect :rest :return :round :safeguard :seed-bomb :sleep-talk :sludge-bomb :snatch :snore :solarbeam :spore :stun-spore :substitute :sunny-day :swagger :swords-dance :synthesis :tackle :toxic :venoshock :wake-up-slap :worry-seed :body-slam :double-edge :mimic :refresh]
  :breloom [:brick-break :bulk-up :counter :cut :dynamicpunch :fling :focus-blast :force-palm :giga-impact :hyper-beam :iron-tail :low-sweep :mach-punch :mind-reader :retaliate :rock-slide :rock-smash :rock-tomb :sky-uppercut :stone-edge :strength :superpower :thunderpunch :work-up])
