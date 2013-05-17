
(in-ns 'smogon.dex)


(defpokemon :grimer
  :name "Grimer"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:stench :sticky-hold]
              :bw [:poison-touch :stench :sticky-hold]]
  :stats [[80 80 50 40 25]
          :gs [80 80 50 40 50 25]]
  :weight [30]
  :height [0.9])

(defpokemon :muk
  :name "Muk"
  :introduced-in :rb
  :types [[:poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:stench :sticky-hold]
              :bw [:poison-touch :stench :sticky-hold]]
  :stats [[105 105 75 65 50]
          :gs [105 105 75 65 100 50]]
  :weight [30]
  :height [1.2])

(deffamily (familychain :grimer :muk))

(deflearnset :rb
  :grimer [:acid-armor :bide :body-slam :disable :double-team :explosion :fire-blast :harden :mega-drain :mimic :minimize :poison-gas :pound :rage :rest :screech :selfdestruct :sludge :substitute :thunder :thunderbolt :toxic]
  :muk [:hyper-beam])

(deflearnset :gs
  :grimer [:acid-armor :attract :curse :disable :double-team :dynamicpunch :endure :fire-blast :fire-punch :flamethrower :frustration :giga-drain :harden :haze :hidden-power :ice-punch :lick :mean-look :minimize :mud-slap :poison-gas :pound :protect :rest :return :screech :sleep-talk :sludge :sludge-bomb :snore :sunny-day :swagger :thief :thunder :thunderbolt :thunderpunch :toxic :zap-cannon]
  :muk [:hyper-beam])

(deflearnset :rs
  :grimer [:acid-armor :attract :body-slam :curse :dig :disable :double-team :dynamicpunch :endure :explosion :facade :fire-blast :fire-punch :flamethrower :frustration :giga-drain :harden :haze :hidden-power :ice-punch :imprison :lick :mean-look :memento :mimic :minimize :mud-slap :poison-gas :pound :protect :rain-dance :rest :return :rock-tomb :screech :secret-power :selfdestruct :shadow-punch :shock-wave :sleep-talk :sludge :sludge-bomb :snore :substitute :sunny-day :swagger :taunt :thief :thunder :thunderbolt :thunderpunch :torment :toxic :body-slam :helping-hand :mimic :selfdestruct]
  :muk [:brick-break :focus-punch :hyper-beam :rock-smash :strength :body-slam :mimic :selfdestruct])

(deflearnset :dp
  :grimer [:acid-armor :attract :captivate :curse :dig :disable :double-team :endure :explosion :facade :fire-blast :fire-punch :flamethrower :fling :frustration :giga-drain :gunk-shot :harden :haze :hidden-power :ice-punch :imprison :lick :mean-look :memento :minimize :mud-bomb :mud-slap :natural-gift :pain-split :payback :poison-gas :poison-jab :pound :protect :rain-dance :rest :return :rock-slide :rock-tomb :screech :secret-power :shadow-ball :shadow-punch :shadow-sneak :shock-wave :sleep-talk :sludge :sludge-bomb :snore :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :taunt :thief :thunder :thunderbolt :thunderpunch :torment :toxic :body-slam :helping-hand :mimic :selfdestruct]
  :muk [:block :brick-break :dark-pulse :focus-blast :focus-punch :giga-impact :hyper-beam :rock-smash :body-slam :mimic :selfdestruct])

(deflearnset :bw
  :grimer [:acid-armor :acid-spray :attract :curse :dig :disable :double-team :explosion :facade :fire-blast :fire-punch :flamethrower :fling :frustration :giga-drain :gunk-shot :harden :haze :hidden-power :ice-punch :imprison :incinerate :lick :mean-look :memento :minimize :mud-bomb :mud-slap :pain-split :payback :poison-gas :poison-jab :pound :protect :rain-dance :rest :return :rock-slide :rock-tomb :round :scary-face :screech :shadow-ball :shadow-punch :shadow-sneak :sleep-talk :sludge :sludge-bomb :sludge-wave :snore :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :taunt :thief :thunder :thunderbolt :thunderpunch :torment :toxic :venoshock :body-slam :helping-hand :mimic :selfdestruct]
  :muk [:block :brick-break :dark-pulse :focus-blast :giga-impact :hyper-beam :rock-smash :body-slam :mimic :selfdestruct])
