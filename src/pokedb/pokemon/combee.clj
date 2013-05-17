
(in-ns 'smogon.dex)


(defpokemon :combee
  :name "Combee"
  :introduced-in :dp
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:honey-gather :hustle]]
  :stats [[30 30 42 30 42 70]]
  :weight [5.5]
  :height [0.3])

(defpokemon :vespiquen
  :name "Vespiquen"
  :introduced-in :dp
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:pressure :unnerve]]
  :stats [[70 80 102 80 102 40]]
  :weight [38.5]
  :height [1.2])

(deffamily (familychain :combee :vespiquen))

(deflearnset :dp
  :combee [:air-cutter :bug-bite :endeavor :gust :mud-slap :ominous-wind :snore :string-shot :sweet-scent :swift :tailwind]
  :vespiquen [:aerial-ace :attack-order :attract :captivate :confuse-ray :cut :defend-order :defog :destiny-bond :double-team :endure :facade :flash :fling :frustration :fury-cutter :fury-swipes :giga-impact :heal-order :hidden-power :hyper-beam :natural-gift :poison-sting :power-gem :protect :pursuit :rain-dance :rest :return :roost :secret-power :signal-beam :silver-wind :slash :sleep-talk :sludge-bomb :substitute :sunny-day :swagger :thief :toxic :u-turn :x-scissor])

(deflearnset :bw
  :combee [:bug-bite :bug-buzz :endeavor :gust :snore :sweet-scent :tailwind]
  :vespiquen [:acrobatics :aerial-ace :air-slash :attack-order :attract :captivate :confuse-ray :cut :defend-order :destiny-bond :double-team :facade :flash :fling :frustration :fury-cutter :fury-swipes :giga-impact :heal-order :hidden-power :hone-claws :hyper-beam :poison-sting :power-gem :protect :pursuit :quash :rain-dance :rest :return :roost :round :signal-beam :slash :sleep-talk :sludge-bomb :struggle-bug :substitute :sunny-day :swagger :thief :toxic :u-turn :venoshock :x-scissor])
