
(in-ns 'smogon.dex)

(defpokemon :seel
  :name "Seel"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:thick-fat]
              :dp [:hydration :thick-fat]
              :bw [:hydration :ice-body :thick-fat]]
  :stats [[65 45 55 70 45]
          :gs [65 45 55 45 70 45]]
  :weight [90]
  :height [1.1])

(defpokemon :dewgong
  :name "Dewgong"
  :introduced-in :rb
  :types [[:ice :water]]
  :egggroups [[:ground :water-1]]
  :abilities [[:thick-fat]
              :dp [:hydration :thick-fat]
              :bw [:hydration :ice-body :thick-fat]]
  :stats [[90 70 80 95 70]
          :gs [90 70 80 70 95 70]]
  :weight [120]
  :height [1.7])

(deffamily (familychain :seel :dewgong))

(deflearnset :rb
  :seel [:aurora-beam :bide :blizzard :body-slam :bubblebeam :double-edge :double-team :growl :headbutt :horn-drill :ice-beam :mimic :pay-day :rage :rest :skull-bash :strength :substitute :surf :take-down :toxic :water-gun :flail]
  :dewgong [:hyper-beam])

(deflearnset :gs
  :seel [:attract :aurora-beam :blizzard :curse :disable :double-team :encore :endure :frustration :growl :headbutt :hidden-power :ice-beam :icy-wind :lick :peck :perish-song :protect :rain-dance :rest :return :safeguard :slam :sleep-talk :snore :surf :swagger :take-down :toxic :waterfall :whirlpool :flail]
  :dewgong [:hyper-beam])

(deflearnset :rs
  :seel [:attract :aurora-beam :blizzard :body-slam :disable :dive :double-edge :double-team :encore :endure :facade :fake-out :frustration :growl :hail :headbutt :hidden-power :horn-drill :ice-beam :icicle-spear :icy-wind :lick :mimic :perish-song :protect :rain-dance :rest :return :safeguard :secret-power :slam :sleep-talk :snore :substitute :surf :swagger :take-down :thief :toxic :water-pulse :waterfall :helping-hand :body-slam :double-edge :helping-hand :mimic]
  :dewgong [:hyper-beam :sheer-cold :signal-beam])

(deflearnset :dp
  :seel [:aqua-jet :aqua-ring :aqua-tail :attract :aurora-beam :blizzard :brine :captivate :disable :dive :double-team :encore :endure :facade :fake-out :fling :frustration :growl :hail :headbutt :hidden-power :horn-drill :ice-beam :ice-shard :icicle-spear :icy-wind :lick :natural-gift :perish-song :protect :rain-dance :rest :return :safeguard :secret-power :signal-beam :slam :sleep-talk :snore :spit-up :stockpile :substitute :surf :swagger :swallow :take-down :thief :toxic :water-pulse :water-sport :waterfall :whirlpool :helping-hand :body-slam :double-edge :helping-hand :mimic]
  :dewgong [:avalanche :giga-impact :hyper-beam :sheer-cold])

(deflearnset :bw
  :seel [:aqua-jet :aqua-ring :aqua-tail :attract :aurora-beam :blizzard :brine :disable :dive :double-team :drill-run :echoed-voice :encore :facade :fake-out :fling :frustration :growl :hail :headbutt :hidden-power :horn-drill :ice-beam :ice-shard :icicle-spear :icy-wind :iron-tail :lick :perish-song :protect :rain-dance :rest :return :round :safeguard :signal-beam :slam :sleep-talk :snore :spit-up :stockpile :substitute :surf :swagger :swallow :take-down :thief :toxic :water-pulse :water-sport :waterfall :helping-hand :body-slam :double-edge :helping-hand :mimic]
  :dewgong [:frost-breath :giga-impact :hyper-beam :sheer-cold])
