
(in-ns 'smogon.dex)


(defpokemon :horsea
  :name "Horsea"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:swift-swim]
              :dp [:sniper :swift-swim]
              :bw [:damp :sniper :swift-swim]]
  :stats [[30 40 70 70 60]
          :gs [30 40 70 70 25 60]]
  :weight [8]
  :height [0.4])

(defpokemon :seadra
  :name "Seadra"
  :introduced-in :rb
  :types [[:water]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:poison-point]
              :dp [:poison-point :sniper]
              :bw [:damp :poison-point :sniper]]
  :stats [[55 65 95 95 85]
          :gs [55 65 95 95 45 85]]
  :weight [25]
  :height [1.2])

(defpokemon :kingdra
  :name "Kingdra"
  :introduced-in :gs
  :types [[:dragon :water]]
  :egggroups [[:dragon :water-1]]
  :abilities [[:swift-swim]
              :dp [:sniper :swift-swim]
              :bw [:damp :sniper :swift-swim]]
  :stats [[75 95 95 95 95 85]]
  :weight [152]
  :height [1.8])

(deffamily (familychain :horsea :seadra :kingdra))

(deflearnset :rb
  :horsea [:agility :bide :blizzard :bubble :bubblebeam :double-edge :double-team :hydro-pump :ice-beam :leer :mimic :rage :rest :skull-bash :smokescreen :substitute :surf :swift :take-down :toxic :water-gun]
  :seadra [:hyper-beam])

(deflearnset :gs
  :horsea [:agility :attract :aurora-beam :blizzard :bubble :curse :disable :double-team :dragon-rage :dragonbreath :endure :flail :frustration :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :leer :octazooka :protect :rain-dance :rest :return :sleep-talk :smokescreen :snore :splash :surf :swagger :swift :toxic :twister :water-gun :waterfall :whirlpool]
  :seadra [:hyper-beam])

(deflearnset :rs
  :horsea [:agility :attract :aurora-beam :blizzard :bubble :disable :dive :double-edge :double-team :dragon-dance :dragon-rage :dragonbreath :endure :facade :flail :frustration :hail :hidden-power :hydro-pump :ice-beam :icy-wind :leer :mimic :octazooka :protect :rain-dance :rest :return :secret-power :sleep-talk :smokescreen :snore :splash :substitute :surf :swagger :swift :toxic :twister :water-gun :water-pulse :waterfall :double-edge :icy-wind :mimic]
  :seadra [:hyper-beam :double-edge :icy-wind :mimic]
  :kingdra [:body-slam :body-slam :double-edge :icy-wind :mimic])

(deflearnset :dp
  :horsea [:agility :attract :aurora-beam :blizzard :bounce :brine :bubble :bubblebeam :captivate :disable :dive :double-team :dragon-dance :dragon-pulse :dragon-rage :dragonbreath :endure :facade :flail :flash-cannon :focus-energy :frustration :hail :headbutt :hidden-power :hydro-pump :ice-beam :icy-wind :leer :muddy-water :natural-gift :octazooka :outrage :protect :rain-dance :razor-wind :rest :return :secret-power :signal-beam :sleep-talk :smokescreen :snore :splash :substitute :surf :swagger :swift :toxic :twister :water-gun :water-pulse :waterfall :whirlpool :double-edge :icy-wind :mimic]
  :seadra [:giga-impact :hyper-beam :double-edge :icy-wind :mimic]
  :kingdra [:draco-meteor :iron-head :yawn :body-slam :double-edge :icy-wind :mimic])

(deflearnset :bw
  :horsea [:agility :attract :aurora-beam :blizzard :bounce :brine :bubble :bubblebeam :clear-smog :disable :dive :double-team :dragon-dance :dragon-pulse :dragon-rage :dragonbreath :facade :flail :flash-cannon :focus-energy :frustration :hail :hidden-power :hydro-pump :ice-beam :icy-wind :leer :muddy-water :octazooka :outrage :protect :rain-dance :razor-wind :rest :return :round :scald :signal-beam :sleep-talk :smokescreen :snore :splash :substitute :surf :swagger :toxic :twister :water-gun :water-pulse :waterfall :double-edge :icy-wind :mimic]
  :seadra [:giga-impact :hyper-beam :double-edge :icy-wind :mimic]
  :kingdra [:draco-meteor :iron-head :quash :yawn :body-slam :double-edge :icy-wind :mimic])
