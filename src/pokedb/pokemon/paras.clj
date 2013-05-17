
(in-ns 'smogon.dex)

(defpokemon :paras
  :name "Paras"
  :introduced-in :rb
  :types [[:bug :grass]]
  :egggroups [[:bug :plant]]
  :abilities [[:effect-spore]
              :dp [:dry-skin :effect-spore]
              :bw [:damp :dry-skin :effect-spore]]
  :stats [[35 70 55 55 25]
          :gs [35 70 55 45 55 25]]
  :weight [5.4]
  :height [0.3])

(defpokemon :parasect
  :name "Parasect"
  :introduced-in :rb
  :types [[:bug :grass]]
  :egggroups [[:bug :plant]]
  :abilities [[:effect-spore]
              :dp [:dry-skin :effect-spore]
              :bw [:damp :dry-skin :effect-spore]]
  :stats [[60 95 80 80 30]
          :gs [60 95 80 60 80 30]]
  :weight [29.5]
  :height [1])

(deffamily (familychain :paras :parasect))

(deflearnset :rb
  :paras [:bide :body-slam :cut :dig :double-edge :double-team :growth :leech-life :mega-drain :mimic :rage :reflect :rest :scratch :skull-bash :slash :solarbeam :spore :stun-spore :substitute :swords-dance :take-down :toxic :synthesis]
  :parasect [:hyper-beam])

(deflearnset :gs
  :paras [:attract :counter :curse :cut :dig :double-team :endure :false-swipe :flail :flash :frustration :fury-cutter :giga-drain :growth :hidden-power :leech-life :light-screen :poisonpowder :protect :psybeam :pursuit :rest :return :rock-smash :scratch :screech :slash :sleep-talk :sludge-bomb :snore :solarbeam :spore :stun-spore :sunny-day :swagger :sweet-scent :thief :toxic :synthesis]
  :parasect [:hyper-beam])

(deflearnset :rs
  :paras [:aerial-ace :aromatherapy :attract :body-slam :bullet-seed :counter :cut :dig :double-edge :double-team :endure :facade :false-swipe :flail :flash :frustration :fury-cutter :giga-drain :growth :hidden-power :leech-life :light-screen :mimic :poisonpowder :protect :psybeam :pursuit :rest :return :rock-smash :scratch :screech :secret-power :slash :sleep-talk :sludge-bomb :snore :solarbeam :spore :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :thief :toxic :refresh :body-slam :double-edge :mimic :refresh]
  :parasect [:hyper-beam :body-slam :double-edge :mimic])

(deflearnset :dp
  :paras [:aerial-ace :agility :aromatherapy :attract :brick-break :bug-bite :bullet-seed :captivate :counter :cross-poison :cut :dig :double-team :endure :energy-ball :facade :false-swipe :flail :flash :frustration :fury-cutter :giga-drain :grass-knot :growth :hidden-power :knock-off :leech-life :light-screen :metal-claw :natural-gift :poisonpowder :protect :psybeam :pursuit :rest :return :rock-smash :scratch :screech :secret-power :seed-bomb :slash :sleep-talk :sludge-bomb :snore :solarbeam :spore :string-shot :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :thief :toxic :worry-seed :x-scissor :refresh :body-slam :double-edge :mimic :refresh]
  :parasect [:giga-impact :hyper-beam :body-slam :double-edge :mimic])

(deflearnset :bw
  :paras [:aerial-ace :after-you :agility :aromatherapy :attract :brick-break :bug-bite :counter :cross-poison :cut :dig :double-team :endure :energy-ball :facade :false-swipe :flail :flash :frustration :fury-cutter :giga-drain :grass-knot :growth :hidden-power :hone-claws :knock-off :leech-life :leech-seed :light-screen :metal-claw :natural-gift :poisonpowder :protect :psybeam :pursuit :rage-powder :rest :return :rock-smash :round :scratch :screech :seed-bomb :slash :sleep-talk :sludge-bomb :snore :solarbeam :spore :struggle-bug :stun-spore :substitute :sunny-day :swagger :sweet-scent :swords-dance :synthesis :thief :toxic :venoshock :worry-seed :x-scissor :refresh :body-slam :double-edge :mimic :refresh]
  :parasect [:giga-impact :hyper-beam :body-slam :double-edge :mimic])
