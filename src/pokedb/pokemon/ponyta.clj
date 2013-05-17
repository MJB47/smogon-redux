
(in-ns 'smogon.dex)

(defpokemon :ponyta
  :name "Ponyta"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire :run-away]
              :bw [:flame-body :flash-fire :run-away]]
  :stats [[50 85 55 65 90]
          :gs [50 85 55 65 65 90]]
  :weight [30]
  :height [1])

(defpokemon :rapidash
  :name "Rapidash"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire :run-away]
              :bw [:flame-body :flash-fire :run-away]]
  :stats [[65 100 70 80 105]
          :gs [65 100 70 80 80 105]]
  :weight [95]
  :height [1.7])

(deffamily (familychain :ponyta :rapidash))

(deflearnset :rb
  :ponyta [:agility :bide :body-slam :double-edge :double-team :ember :fire-blast :fire-spin :growl :horn-drill :mimic :rage :reflect :rest :skull-bash :stomp :substitute :swift :tail-whip :take-down :toxic :low-kick]
  :rapidash [:hyper-beam])

(deflearnset :gs
  :ponyta [:agility :attract :charm :curse :double-kick :double-team :ember :endure :fire-blast :fire-spin :flame-wheel :flamethrower :frustration :growl :headbutt :hidden-power :hypnosis :iron-tail :protect :quick-attack :rest :return :sleep-talk :snore :stomp :sunny-day :swagger :swift :tackle :tail-whip :take-down :thrash :toxic :low-kick]
  :rapidash [:fury-attack :hyper-beam])

(deflearnset :rs
  :ponyta [:agility :attract :body-slam :bounce :charm :double-edge :double-kick :double-team :ember :endure :facade :fire-blast :fire-spin :flame-wheel :flamethrower :frustration :growl :hidden-power :hypnosis :iron-tail :mimic :overheat :protect :quick-attack :rest :return :secret-power :sleep-talk :snore :solarbeam :stomp :strength :substitute :sunny-day :swagger :swift :tackle :tail-whip :take-down :thrash :toxic :body-slam :mimic]
  :rapidash [:fury-attack :hyper-beam :baton-pass :baton-pass :body-slam :mimic])

(deflearnset :dp
  :ponyta [:agility :attract :bounce :captivate :charm :double-edge :double-kick :double-team :ember :endure :facade :fire-blast :fire-spin :flame-wheel :flamethrower :flare-blitz :frustration :growl :headbutt :heat-wave :hidden-power :horn-drill :hypnosis :iron-tail :morning-sun :natural-gift :overheat :protect :rest :return :secret-power :sleep-talk :snore :solarbeam :stomp :strength :substitute :sunny-day :swagger :swift :tackle :tail-whip :take-down :thrash :toxic :will-o-wisp :body-slam :mimic]
  :rapidash [:fury-attack :giga-impact :hyper-beam :megahorn :poison-jab :quick-attack :baton-pass :baton-pass :body-slam :mimic])

(deflearnset :bw
  :ponyta [:agility :attract :bounce :captivate :charm :double-edge :double-kick :double-team :echoed-voice :ember :facade :fire-blast :fire-spin :flame-charge :flame-wheel :flamethrower :flare-blitz :frustration :growl :heat-wave :hidden-power :horn-drill :hypnosis :incinerate :inferno :iron-tail :low-kick :morning-sun :overheat :protect :rest :return :round :sleep-talk :snore :solarbeam :stomp :strength :substitute :sunny-day :swagger :tackle :tail-whip :take-down :thrash :toxic :wild-charge :will-o-wisp :body-slam :mimic]
  :rapidash [:drill-run :fury-attack :giga-impact :hyper-beam :megahorn :poison-jab :quick-attack :baton-pass :baton-pass :body-slam :mimic])
