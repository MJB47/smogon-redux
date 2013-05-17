
(in-ns 'smogon.dex)


(defpokemon :torchic
  :name "Torchic"
  :introduced-in :rs
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze]
              :bw [:blaze :speed-boost]]
  :stats [[45 60 40 70 50 45]]
  :weight [2.5]
  :height [0.4])

(defpokemon :combusken
  :name "Combusken"
  :introduced-in :rs
  :types [[:fighting :fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze]
              :bw [:blaze :speed-boost]]
  :stats [[60 85 60 85 60 55]]
  :weight [19.5]
  :height [0.9])

(defpokemon :blaziken
  :name "Blaziken"
  :introduced-in :rs
  :types [[:fighting :fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze]
              :bw [:blaze :speed-boost]]
  :stats [[80 120 70 110 70 80]]
  :weight [52]
  :height [1.9])

(deffamily (familychain :torchic :combusken :blaziken))



(deflearnset :rs
  :torchic [:aerial-ace :attract :body-slam :counter :cut :dig :double-edge :double-team :ember :endure :facade :fire-blast :fire-spin :flamethrower :focus-energy :frustration :growl :hidden-power :mega-kick :mega-punch :mimic :mirror-move :mud-slap :overheat :peck :protect :quick-attack :rest :return :reversal :rock-slide :rock-smash :rock-tomb :sand-attack :scratch :secret-power :seismic-toss :slash :sleep-talk :smellingsalt :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :toxic :body-slam :double-edge :mimic :seismic-toss]
  :combusken [:brick-break :bulk-up :double-kick :dynamicpunch :fire-punch :focus-punch :fury-cutter :sky-uppercut :thunderpunch]
  :blaziken [:blaze-kick :earthquake :hyper-beam :roar])

(deflearnset :dp
  :torchic [:aerial-ace :agility :attract :baton-pass :bounce :captivate :counter :crush-claw :cut :dig :double-team :ember :endure :facade :featherdance :feint :fire-blast :fire-spin :flamethrower :focus-energy :frustration :growl :headbutt :heat-wave :helping-hand :hidden-power :last-resort :mirror-move :mud-slap :natural-gift :night-slash :overheat :peck :protect :quick-attack :rest :return :reversal :rock-slide :rock-smash :rock-tomb :sand-attack :scratch :secret-power :shadow-claw :slash :sleep-talk :smellingsalt :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :toxic :will-o-wisp :body-slam :double-edge :mimic :seismic-toss]
  :combusken [:brick-break :bulk-up :double-kick :fire-punch :flare-blitz :fling :focus-blast :focus-punch :fury-cutter :low-kick :poison-jab :sky-uppercut :thunderpunch :vacuum-wave]
  :blaziken [:blast-burn :blaze-kick :brave-bird :earthquake :giga-impact :hyper-beam :knock-off :roar :rock-climb :role-play :solarbeam :stone-edge :superpower])

(deflearnset :bw
  :torchic [:aerial-ace :agility :attract :baton-pass :bounce :counter :crush-claw :curse :cut :dig :double-team :echoed-voice :ember :endure :facade :featherdance :feint :fire-blast :fire-pledge :fire-spin :flame-burst :flame-charge :flamethrower :focus-energy :frustration :growl :heat-wave :helping-hand :hidden-power :hone-claws :incinerate :last-resort :low-kick :mirror-move :night-slash :overheat :peck :protect :quick-attack :rest :return :reversal :rock-slide :rock-smash :rock-tomb :round :sand-attack :scratch :shadow-claw :slash :sleep-talk :smellingsalt :snore :strength :substitute :sunny-day :swagger :swords-dance :toxic :will-o-wisp :body-slam :double-edge :mimic :seismic-toss]
  :combusken [:brick-break :bulk-up :double-kick :dual-chop :fire-punch :flare-blitz :fling :focus-blast :low-sweep :poison-jab :sky-uppercut :thunderpunch :work-up]
  :blaziken [:acrobatics :blast-burn :blaze-kick :brave-bird :bulldoze :earthquake :giga-impact :hi-jump-kick :hyper-beam :knock-off :roar :role-play :solarbeam :stone-edge :superpower])

