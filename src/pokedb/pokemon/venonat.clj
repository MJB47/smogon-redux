
(in-ns 'smogon.dex)


(defpokemon :venonat
  :name "Venonat"
  :introduced-in :rb
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:compoundeyes]
              :dp [:compoundeyes :tinted-lens]
              :bw [:compoundeyes :run-away :tinted-lens]]
  :stats [[60 55 50 40 45]
          :gs [60 55 50 40 55 45]]
  :weight [30]
  :height [1])

(defpokemon :venomoth
  :name "Venomoth"
  :introduced-in :rb
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:shield-dust]
              :dp [:shield-dust :tinted-lens]
              :bw [:shield-dust :tinted-lens :wonder-skin]]
  :stats [[70 65 60 90 90]
          :gs [70 65 60 90 75 90]]
  :weight [12.5]
  :height [1.5])

(deffamily (familychain :venonat :venomoth))



(deflearnset :rb
  :venonat [:bide :confusion :disable :double-edge :double-team :flash :leech-life :mega-drain :mimic :poisonpowder :psybeam :psychic :psywave :rage :reflect :rest :sleep-powder :solarbeam :stun-spore :substitute :supersonic :tackle :take-down :toxic]
  :venomoth [:hyper-beam :razor-wind :swift :teleport :whirlwind])

(deflearnset :gs
  :venonat [:attract :baton-pass :confusion :curse :disable :double-team :endure :foresight :frustration :giga-drain :hidden-power :leech-life :poisonpowder :protect :psybeam :psychic :rest :return :screech :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :sunny-day :supersonic :swagger :sweet-scent :swift :tackle :thief :toxic]
  :venomoth [:flash :gust :hyper-beam])

(deflearnset :rs
  :venonat [:attract :baton-pass :confusion :disable :double-edge :double-team :endure :facade :flash :foresight :frustration :giga-drain :hidden-power :leech-life :mimic :poisonpowder :protect :psybeam :psychic :rest :return :screech :secret-power :signal-beam :skill-swap :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :stun-spore :substitute :sunny-day :supersonic :swagger :swift :tackle :thief :toxic :double-edge :mimic]
  :venomoth [:aerial-ace :gust :hyper-beam :silver-wind :double-edge :mimic :refresh])

(deflearnset :dp
  :venonat [:agility :attract :baton-pass :bug-bite :captivate :confusion :disable :double-team :endure :facade :flash :foresight :frustration :giga-drain :hidden-power :leech-life :morning-sun :natural-gift :poison-fang :poisonpowder :protect :psybeam :psychic :rest :return :screech :secret-power :signal-beam :skill-swap :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :string-shot :stun-spore :substitute :sunny-day :supersonic :swagger :swift :tackle :thief :toxic :toxic-spikes :zen-headbutt :double-edge :mimic]
  :venomoth [:aerial-ace :air-cutter :bug-buzz :defog :energy-ball :giga-impact :gust :hyper-beam :ominous-wind :roost :silver-wind :tailwind :twister :u-turn :double-edge :mimic :refresh])

(deflearnset :bw
  :venonat [:agility :attract :baton-pass :bug-bite :confusion :disable :double-team :facade :flash :foresight :frustration :giga-drain :hidden-power :leech-life :morning-sun :poison-fang :poisonpowder :protect :psybeam :psychic :rage-powder :rest :return :round :screech :secret-power :signal-beam :skill-swap :sleep-powder :sleep-talk :sludge-bomb :snore :solarbeam :struggle-bug :stun-spore :substitute :sunny-day :supersonic :swagger :tackle :thief :toxic :toxic-spikes :venoshock :zen-headbutt :double-edge :mimic]
  :venomoth [:acrobatics :aerial-ace :bug-buzz :energy-ball :giga-impact :gust :hyper-beam :quiver-dance :roost :silver-wind :tailwind :u-turn :double-edge :mimic :refresh])

