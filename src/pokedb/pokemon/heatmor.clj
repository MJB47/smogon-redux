
(in-ns 'smogon.dex)


(defpokemon :heatmor
  :name "Heatmor"
  :introduced-in :bw
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire :gluttony :white-smoke]]
  :stats [[85 97 66 105 66 65]]
  :weight [58]
  :height [1.4])

(deffamily (familychain :heatmor))

(deflearnset :bw
  :heatmor [:aerial-ace :amnesia :attract :bind :body-slam :bug-bite :curse :cut :dig :double-team :facade :faint-attack :fire-blast :fire-punch :fire-spin :flame-burst :flamethrower :fling :focus-blast :frustration :fury-swipes :gastro-acid :giga-drain :giga-impact :heat-wave :hidden-power :hone-claws :incinerate :inferno :knock-off :lick :low-kick :night-slash :odor-sleuth :protect :pursuit :rain-dance :recycle :rest :return :rock-smash :rock-tomb :round :shadow-claw :slash :sleep-talk :snatch :snore :solarbeam :spit-up :stockpile :substitute :sucker-punch :sunny-day :superpower :swagger :swallow :taunt :thief :thunderpunch :tickle :toxic :will-o-wisp :wrap])

