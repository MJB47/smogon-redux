
(in-ns 'smogon.dex)


(defpokemon :azurill
  :name "Azurill"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[]]
  :abilities [[:huge-power :thick-fat]
              :bw [:huge-power :sap-sipper :thick-fat]]
  :stats [[50 20 40 20 40 20]]
  :weight [2]
  :height [0.2])

(defpokemon :marill
  :name "Marill"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:fairy :water-1]]
  :abilities [[:huge-power :thick-fat]
              :bw [:huge-power :sap-sipper :thick-fat]]
  :stats [[70 20 50 20 50 40]]
  :weight [8.5]
  :height [0.4])

(defpokemon :azumarill
  :name "Azumarill"
  :introduced-in :gs
  :types [[:water]]
  :egggroups [[:fairy :water-1]]
  :abilities [[:huge-power :thick-fat]
              :bw [:huge-power :sap-sipper :thick-fat]]
  :stats [[100 50 80 50 80 50]]
  :weight [28.5]
  :height [0.8])

(deffamily (familychain :azurill :marill :azumarill))

(deflearnset :gs
  :marill [:amnesia :attract :belly-drum :blizzard :bubblebeam :curse :defense-curl :double-edge :double-team :dynamicpunch :endure :foresight :frustration :future-sight :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :light-screen :mud-slap :perish-song :present :protect :rain-dance :rest :return :rollout :sleep-talk :snore :supersonic :surf :swagger :swift :tackle :tail-whip :toxic :water-gun :waterfall :whirlpool]
  :azumarill [:hyper-beam :rock-smash :strength])

(deflearnset :rs
  :azurill [:attract :blizzard :body-slam :bubble :charm :defense-curl :double-edge :double-team :encore :endure :facade :frustration :hail :hidden-power :ice-beam :icy-wind :iron-tail :mimic :mud-slap :protect :rain-dance :refresh :rest :return :rollout :secret-power :sing :slam :sleep-talk :snore :splash :substitute :surf :swagger :swift :tail-whip :tickle :toxic :water-gun :water-pulse :waterfall]
  :marill [:amnesia :belly-drum :brick-break :bubblebeam :dig :dive :dynamicpunch :focus-punch :future-sight :hydro-pump :ice-punch :light-screen :mega-kick :mega-punch :perish-song :present :rock-smash :seismic-toss :strength :supersonic :tackle]
  :azumarill [:hyper-beam])

(deflearnset :dp
  :azurill [:attract :blizzard :body-slam :bubble :captivate :charm :double-team :encore :endure :facade :fake-tears :frustration :hail :headbutt :helping-hand :hidden-power :ice-beam :icy-wind :iron-tail :knock-off :mud-slap :natural-gift :protect :rain-dance :refresh :rest :return :rollout :secret-power :sing :slam :sleep-talk :snore :splash :substitute :surf :swagger :swift :tail-whip :tickle :toxic :uproar :water-gun :water-pulse :waterfall :whirlpool]
  :marill [:amnesia :aqua-jet :aqua-ring :aqua-tail :belly-drum :brick-break :bubblebeam :defense-curl :dig :dive :double-edge :fling :focus-punch :future-sight :grass-knot :hydro-pump :ice-punch :light-screen :perish-song :present :rock-smash :strength :superpower :supersonic :tackle]
  :azumarill [:focus-blast :giga-impact :hyper-beam])

(deflearnset :bw
  :azurill [:attract :blizzard :body-slam :bounce :bubble :bubblebeam :charm :covet :double-team :encore :facade :fake-tears :frustration :hail :helping-hand :hidden-power :hyper-voice :ice-beam :icy-wind :iron-tail :knock-off :light-screen :muddy-water :protect :rain-dance :refresh :rest :return :round :scald :sing :slam :sleep-talk :snore :soak :splash :substitute :surf :swagger :tail-whip :tickle :toxic :uproar :water-gun :water-sport :waterfall :work-up]
  :marill [:amnesia :aqua-jet :aqua-ring :aqua-tail :belly-drum :brick-break :defense-curl :dig :dive :double-edge :fling :future-sight :grass-knot :hydro-pump :ice-punch :perish-song :present :rock-smash :rollout :strength :superpower :supersonic :tackle]
  :azumarill [:bulldoze :focus-blast :giga-impact :hyper-beam])
