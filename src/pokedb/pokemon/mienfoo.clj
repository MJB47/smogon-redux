
(in-ns 'smogon.dex)


(defpokemon :mienfoo
  :name "Mienfoo"
  :introduced-in :bw
  :types [[:fighting]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:inner-focus :reckless :regenerator]]
  :stats [[45 85 50 55 50 65]]
  :weight [20]
  :height [0.9])

(defpokemon :mienshao
  :name "Mienshao"
  :introduced-in :bw
  :types [[:fighting]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:inner-focus :reckless :regenerator]]
  :stats [[65 125 60 95 60 105]]
  :weight [35.5]
  :height [1.4])

(deffamily (familychain :mienfoo :mienshao))

(deflearnset :bw
  :mienfoo [:acrobatics :aerial-ace :attract :aura-sphere :baton-pass :bounce :brick-break :bulk-up :calm-mind :detect :dig :double-team :doubleslap :drain-punch :dual-chop :endure :facade :fake-out :feint :fling :focus-blast :force-palm :frustration :grass-knot :helping-hand :hi-jump-kick :hidden-power :jump-kick :knock-off :low-kick :low-sweep :me-first :meditate :payback :poison-jab :pound :protect :psych-up :quick-guard :rain-dance :reflect :rest :retaliate :return :reversal :rock-slide :rock-smash :rock-tomb :role-play :round :sleep-talk :smellingsalt :snore :stone-edge :strength :substitute :sunny-day :swagger :swift :swords-dance :taunt :toxic :u-turn :vital-throw :work-up]
  :mienshao [:giga-impact :hyper-beam :wide-guard])
