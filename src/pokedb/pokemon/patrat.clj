
(in-ns 'smogon.dex)


(defpokemon :patrat
  :name "Patrat"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:analytic :keen-eye :run-away]]
  :stats [[45 55 39 35 39 42]]
  :weight [11.6]
  :height [0.5])

(defpokemon :watchog
  :name "Watchog"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:analytic :illuminate :keen-eye]]
  :stats [[60 85 69 60 69 77]]
  :weight [27]
  :height [1.1])

(deffamily (familychain :patrat :watchog))

(deflearnset :bw
  :patrat [:after-you :aqua-tail :assurance :attract :baton-pass :bide :bite :covet :crunch :cut :detect :dig :double-team :endeavor :facade :flail :fling :foresight :frustration :grass-knot :gunk-shot :helping-hand :hidden-power :hyper-fang :hypnosis :iron-tail :last-resort :leer :low-kick :mean-look :protect :pursuit :rain-dance :rest :retaliate :return :revenge :round :sand-attack :screech :seed-bomb :shadow-ball :slam :sleep-talk :snore :substitute :sunny-day :super-fang :swagger :swords-dance :tackle :thunderbolt :toxic :work-up :zen-headbutt]
  :watchog [:confuse-ray :dream-eater :fire-punch :flamethrower :flash :focus-blast :giga-impact :hyper-beam :ice-punch :knock-off :light-screen :psych-up :rock-smash :signal-beam :strength :thunder :thunder-wave :thunderpunch])
