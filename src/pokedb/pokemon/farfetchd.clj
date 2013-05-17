
(in-ns 'smogon.dex)

(defpokemon :farfetchd
  :name "Farfetch'd"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying :ground]]
  :abilities [[:inner-focus :keen-eye]
              :bw [:defiant :inner-focus :keen-eye]]
  :stats [[52 65 55 58 60]
          :gs [52 65 55 58 62 60]]
  :weight [15]
  :height [0.8])

(deffamily (familychain :farfetchd))

(deflearnset :rb
  :farfetchd [:agility :bide :body-slam :cut :double-edge :double-team :fly :fury-attack :leer :mimic :peck :rage :razor-wind :reflect :rest :sand-attack :skull-bash :slash :substitute :swift :swords-dance :take-down :toxic :whirlwind :fury-cutter])

(deflearnset :gs
  :farfetchd [:agility :attract :curse :cut :detect :double-team :endure :false-swipe :flail :fly :foresight :frustration :fury-attack :gust :headbutt :hidden-power :iron-tail :leer :mirror-move :mud-slap :peck :protect :psych-up :quick-attack :rest :return :sand-attack :slash :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :swords-dance :thief :toxic :fury-cutter])

(deflearnset :rs
  :farfetchd [:aerial-ace :agility :attract :body-slam :curse :cut :double-edge :double-team :endure :facade :false-swipe :featherdance :flail :fly :foresight :frustration :fury-attack :fury-cutter :gust :hidden-power :iron-tail :knock-off :leer :mimic :mirror-move :mud-slap :peck :protect :psych-up :quick-attack :rest :return :sand-attack :secret-power :slash :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :swords-dance :thief :toxic :wish :yawn :baton-pass :baton-pass :body-slam :double-edge :mimic])

(deflearnset :dp
  :farfetchd [:aerial-ace :agility :air-cutter :air-slash :attract :captivate :covet :curse :cut :defog :double-team :endure :facade :false-swipe :featherdance :feint :flail :fly :foresight :frustration :fury-attack :fury-cutter :gust :headbutt :heat-wave :hidden-power :iron-tail :knock-off :last-resort :leaf-blade :leer :mirror-move :mud-slap :natural-gift :night-slash :ominous-wind :peck :pluck :poison-jab :protect :psych-up :quick-attack :rest :return :roost :sand-attack :secret-power :slash :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :swords-dance :thief :toxic :twister :u-turn :uproar :wish :yawn :baton-pass :baton-pass :body-slam :double-edge :mimic])

(deflearnset :bw
  :farfetchd [:acrobatics :aerial-ace :agility :air-cutter :air-slash :attract :brave-bird :covet :curse :cut :double-team :facade :false-swipe :featherdance :feint :flail :fly :foresight :frustration :fury-attack :fury-cutter :gust :heat-wave :helping-hand :hidden-power :iron-tail :knock-off :last-resort :leaf-blade :leer :mirror-move :mud-slap :night-slash :peck :pluck :poison-jab :protect :psych-up :quick-attack :rest :retaliate :return :revenge :roost :round :sand-attack :sky-attack :slash :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swords-dance :tailwind :thief :toxic :trump-card :u-turn :uproar :work-up :wish :yawn :baton-pass :baton-pass :body-slam :double-edge :mimic])
