
(in-ns 'smogon.dex)


(defpokemon :growlithe
  :name "Growlithe"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire :intimidate]
              :bw [:flash-fire :intimidate :justified]]
  :stats [[55 70 45 50 60]
          :gs [55 70 45 70 50 60]]
  :weight [19]
  :height [0.7])

(defpokemon :arcanine
  :name "Arcanine"
  :introduced-in :rb
  :types [[:fire]]
  :egggroups [[:ground]]
  :abilities [[:flash-fire :intimidate]
              :bw [:flash-fire :intimidate :justified]]
  :stats [[90 110 80 80 95]
          :gs [90 110 80 100 80 95]]
  :weight [155]
  :height [1.9])

(deffamily (familychain :growlithe :arcanine))

(deflearnset :rb
  :growlithe [:agility :bide :bite :body-slam :dig :double-edge :double-team :dragon-rage :ember :fire-blast :flamethrower :leer :mimic :rage :reflect :rest :roar :skull-bash :substitute :swift :take-down :toxic]
  :arcanine [:hyper-beam :teleport])

(deflearnset :gs
  :growlithe [:agility :attract :bite :body-slam :crunch :curse :dig :double-team :dragonbreath :ember :endure :fire-blast :fire-spin :flame-wheel :flamethrower :frustration :headbutt :hidden-power :iron-tail :leer :protect :rest :return :roar :rock-smash :safeguard :sleep-talk :snore :sunny-day :swagger :swift :take-down :thrash :toxic]
  :arcanine [:extremespeed :hyper-beam])

(deflearnset :rs
  :growlithe [:aerial-ace :agility :attract :bite :body-slam :crunch :dig :double-edge :double-team :ember :endure :facade :fire-blast :fire-spin :flame-wheel :flamethrower :frustration :heat-wave :helping-hand :hidden-power :howl :iron-tail :leer :mimic :odor-sleuth :overheat :protect :rest :return :roar :rock-smash :safeguard :secret-power :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :take-down :thief :thrash :toxic :charm :mimic]
  :arcanine [:extremespeed :hyper-beam])

(deflearnset :dp
  :growlithe [:aerial-ace :agility :attract :bite :body-slam :captivate :crunch :dig :double-edge :double-team :ember :endure :facade :fire-blast :fire-fang :fire-spin :flame-wheel :flamethrower :flare-blitz :frustration :headbutt :heat-wave :helping-hand :hidden-power :howl :iron-tail :leer :morning-sun :mud-slap :natural-gift :odor-sleuth :overheat :protect :rest :return :reversal :roar :rock-smash :safeguard :secret-power :sleep-talk :snore :strength :substitute :sunny-day :swagger :swift :take-down :thief :thrash :toxic :will-o-wisp :charm :mimic]
  :arcanine [:dragon-pulse :extremespeed :giga-impact :hyper-beam :iron-head :rock-climb :solarbeam :thunder-fang])

(deflearnset :bw
  :growlithe [:aerial-ace :agility :attract :bite :body-slam :close-combat :covet :crunch :dig :double-edge :double-kick :double-team :ember :facade :fire-blast :fire-fang :fire-spin :flame-burst :flame-charge :flame-wheel :flamethrower :flare-blitz :frustration :heat-wave :helping-hand :hidden-power :howl :incinerate :iron-tail :leer :morning-sun :odor-sleuth :outrage :overheat :protect :rest :retaliate :return :reversal :roar :rock-smash :round :safeguard :sleep-talk :snarl :snore :strength :substitute :sunny-day :swagger :take-down :thief :thrash :toxic :wild-charge :will-o-wisp :charm :mimic]
  :arcanine [:bulldoze :dragon-pulse :extremespeed :giga-impact :hyper-beam :iron-head :solarbeam :thunder-fang])
