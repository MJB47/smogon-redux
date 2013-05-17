
(in-ns 'smogon.dex)

(defpokemon :caterpie
  :name "Caterpie"
  :introduced-in :rb
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:shield-dust]
              :bw [:run-away :shield-dust]]
  :stats [[45 30 35 20 45]
          :gs [45 30 35 20 20 45]]
  :weight [2.9]
  :height [0.3])

(defpokemon :metapod
  :name "Metapod"
  :introduced-in :rb
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:shed-skin]]
  :stats [[50 20 55 25 30]
          :gs [50 20 55 25 25 30]]
  :weight [9.9]
  :height [0.7])

(defpokemon :butterfree
  :name "Butterfree"
  :introduced-in :rb
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:compoundeyes]
              :bw [:compoundeyes :tinted-lens]]
  :stats [[60 45 50 80 70]
          :gs [60 45 50 80 80 70]]
  :weight [32]
  :height [1.1])

(deffamily (familychain :caterpie :metapod :butterfree))

(deflearnset :rb
  :caterpie [:string-shot :tackle]
  :metapod [:harden]
  :butterfree [:bide :confusion :double-edge :double-team :flash :gust :hyper-beam :mega-drain :mimic :poisonpowder :psybeam :psychic :psywave :rage :razor-wind :reflect :rest :sleep-powder :solarbeam :stun-spore :substitute :supersonic :swift :take-down :teleport :toxic :whirlwind])

(deflearnset :gs
  :caterpie [:string-shot :tackle]
  :metapod [:harden]
  :butterfree [:attract :confusion :curse :double-team :endure :flash :frustration :giga-drain :gust :hidden-power :hyper-beam :nightmare :poisonpowder :protect :psybeam :psychic :rest :return :safeguard :sleep-powder :sleep-talk :snore :solarbeam :stun-spore :sunny-day :supersonic :swagger :sweet-scent :swift :toxic :whirlwind])

(deflearnset :rs
  :caterpie [:string-shot :tackle]
  :metapod [:harden]
  :butterfree [:aerial-ace :attract :confusion :double-edge :double-team :dream-eater :endure :facade :flash :frustration :giga-drain :gust :hidden-power :hyper-beam :mimic :morning-sun :nightmare :poisonpowder :protect :psybeam :psychic :rain-dance :rest :return :safeguard :secret-power :shadow-ball :silver-wind :skill-swap :sleep-powder :sleep-talk :snore :solarbeam :stun-spore :substitute :sunny-day :supersonic :swagger :swift :thief :toxic :whirlwind])

(deflearnset :dp
  :caterpie [:bug-bite :snore :string-shot :tackle]
  :metapod [:harden :iron-defense]
  :butterfree [:aerial-ace :air-cutter :attract :bug-buzz :captivate :confusion :defog :double-edge :double-team :dream-eater :endure :energy-ball :facade :flash :frustration :giga-drain :giga-impact :gust :hidden-power :hyper-beam :mimic :morning-sun :natural-gift :nightmare :ominous-wind :poisonpowder :protect :psybeam :psych-up :psychic :rain-dance :rest :return :roost :safeguard :secret-power :shadow-ball :signal-beam :silver-wind :skill-swap :sleep-powder :sleep-talk :solarbeam :stun-spore :substitute :sunny-day :supersonic :swagger :swift :tailwind :thief :toxic :twister :u-turn :whirlwind])

(deflearnset :bw
  :caterpie [:bug-bite :electroweb :snore :string-shot :tackle]
  :metapod [:harden :iron-defense]
  :butterfree [:acrobatics :aerial-ace :attract :bug-buzz :captivate :confusion :double-edge :double-team :dream-eater :energy-ball :facade :flash :frustration :giga-drain :giga-impact :gust :hidden-power :hyper-beam :mimic :morning-sun :nightmare :poisonpowder :protect :psybeam :psych-up :psychic :quiver-dance :rage-powder :rain-dance :rest :return :roost :round :safeguard :shadow-ball :signal-beam :silver-wind :skill-swap :sleep-powder :sleep-talk :solarbeam :struggle-bug :stun-spore :substitute :sunny-day :supersonic :swagger :tailwind :thief :toxic :u-turn :venoshock :whirlwind])
