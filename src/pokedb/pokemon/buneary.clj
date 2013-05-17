
(in-ns 'smogon.dex)


(defpokemon :buneary
  :name "Buneary"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:klutz :limber :run-away]]
  :stats [[55 66 44 44 56 85]]
  :weight [5.5]
  :height [0.4])

(defpokemon :lopunny
  :name "Lopunny"
  :introduced-in :dp
  :types [[:normal]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:cute-charm :klutz :limber]]
  :stats [[65 76 84 54 96 105]]
  :weight [33.3]
  :height [1.2])

(deffamily (familychain :buneary :lopunny))

(deflearnset :dp
  :buneary [:agility :attract :baton-pass :bounce :captivate :charge-beam :charm :cut :defense-curl :dig :dizzy-punch :double-hit :double-team :drain-punch :encore :endeavor :endure :facade :fake-out :fake-tears :fire-punch :flail :fling :focus-punch :foresight :frustration :grass-knot :headbutt :heal-bell :healing-wish :helping-hand :hidden-power :ice-beam :ice-punch :iron-tail :jump-kick :last-resort :low-kick :magic-coat :mud-slap :natural-gift :pound :protect :quick-attack :rain-dance :rest :return :rock-smash :secret-power :shadow-ball :shock-wave :sky-uppercut :sleep-talk :snore :solarbeam :splash :substitute :sunny-day :swagger :sweet-kiss :swift :switcheroo :thunder-wave :thunderbolt :thunderpunch :toxic :uproar :water-pulse]
  :lopunny [:blizzard :focus-blast :fury-cutter :giga-impact :hyper-beam :mirror-coat :strength :thunder])

(deflearnset :bw
  :buneary [:after-you :agility :attract :baton-pass :bounce :charge-beam :charm :circle-throw :copycat :covet :cut :defense-curl :dig :dizzy-punch :double-hit :double-team :drain-punch :encore :endeavor :endure :entrainment :facade :fake-out :fake-tears :fire-punch :flail :fling :focus-punch :foresight :frustration :grass-knot :heal-bell :healing-wish :helping-hand :hidden-power :hyper-voice :ice-beam :ice-punch :iron-tail :jump-kick :last-resort :low-kick :magic-coat :pound :protect :quick-attack :rain-dance :rest :retaliate :return :rock-smash :round :shadow-ball :sky-uppercut :sleep-talk :snore :solarbeam :splash :substitute :sunny-day :swagger :sweet-kiss :switcheroo :thunder-wave :thunderbolt :thunderpunch :toxic :uproar :work-up]
  :lopunny [:blizzard :focus-blast :giga-impact :hyper-beam :low-sweep :mirror-coat :strength :thunder])
