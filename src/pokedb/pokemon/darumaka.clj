
(in-ns 'smogon.dex)


(defpokemon :darumaka
  :name "Darumaka"
  :introduced-in :bw
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:hustle :inner-focus]]
  :stats [[70 90 45 15 45 50]]
  :weight [37.5]
  :height [0.6])

(defpokemon :darmanitan
  :name "Darmanitan"
  :introduced-in :bw
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:sheer-force :zen-mode]]
  :stats [[105 140 55 30 55 95]]
  :weight [92.9]
  :height [1.3])

(deffamily (familychain :darumaka :darmanitan))

(deflearnset :bw
  :darumaka [:attract :belly-drum :brick-break :dig :double-team :encore :endeavor :endure :facade :fire-blast :fire-fang :fire-punch :flame-charge :flame-wheel :flamethrower :flare-blitz :fling :focus-energy :focus-punch :frustration :grass-knot :gyro-ball :hammer-arm :headbutt :heat-wave :hidden-power :incinerate :overheat :protect :rage :rest :return :roar :rock-slide :rock-smash :rock-tomb :rollout :round :sleep-talk :snore :solarbeam :strength :substitute :sunny-day :superpower :swagger :tackle :take-down :taunt :thief :thrash :toxic :u-turn :uproar :will-o-wisp :work-up :yawn :zen-headbutt]
  :darmanitan [:bulk-up :bulldoze :earthquake :focus-blast :giga-impact :hyper-beam :payback :psychic :smack-down :stone-edge :torment])
