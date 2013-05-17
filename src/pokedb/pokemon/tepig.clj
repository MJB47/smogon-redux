
(in-ns 'smogon.dex)


(defpokemon :tepig
  :name "Tepig"
  :introduced-in :dp
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze :thick-fat]]
  :stats [[65 63 45 45 45 45]]
  :weight [9.9]
  :height [0.5])

(defpokemon :pignite
  :name "Pignite"
  :introduced-in :dp
  :types [[:fighting :fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze :thick-fat]]
  :stats [[90 93 55 70 55 55]]
  :weight [55.5]
  :height [1])

(defpokemon :emboar
  :name "Emboar"
  :introduced-in :dp
  :types [[:fighting :fire]]
  :egggroups [[:ground]]
  :abilities [[:blaze :reckless]]
  :stats [[110 123 65 100 65 65]]
  :weight [150]
  :height [1.6])

(deffamily (familychain :tepig :pignite :emboar))



(deflearnset :bw
  :tepig [:assurance :attract :body-slam :covet :curse :defense-curl :double-team :echoed-voice :ember :endeavor :facade :fire-blast :fire-pledge :flame-charge :flamethrower :flare-blitz :frustration :grass-knot :gyro-ball :head-smash :heat-crash :heat-wave :heavy-slam :helping-hand :hidden-power :incinerate :iron-tail :magnitude :odor-sleuth :overheat :protect :rest :return :roar :rock-smash :rock-tomb :rollout :round :sleep-talk :smog :snore :solarbeam :strength :substitute :sunny-day :superpower :swagger :tackle :tail-whip :take-down :taunt :thrash :toxic :wild-charge :will-o-wisp :yawn]
  :pignite [:arm-thrust :brick-break :bulldoze :fire-punch :fling :focus-blast :low-kick :low-sweep :poison-jab :rock-slide :stone-edge :thunderpunch :work-up]
  :emboar [:blast-burn :block :bulk-up :earthquake :giga-impact :hammer-arm :hyper-beam :iron-head :scald :smack-down])

