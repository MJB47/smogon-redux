
(in-ns 'smogon.dex)


(defpokemon :chimchar
  :name "Chimchar"
  :introduced-in :dp
  :types [[:fire]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:blaze :iron-fist]]
  :stats [[44 58 44 58 44 61]]
  :weight [6.2]
  :height [0.5])

(defpokemon :monferno
  :name "Monferno"
  :introduced-in :dp
  :types [[:fighting :fire]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:blaze :iron-fist]]
  :stats [[64 78 52 78 52 81]]
  :weight [22]
  :height [0.9])

(defpokemon :infernape
  :name "Infernape"
  :introduced-in :dp
  :types [[:fighting :fire]]
  :egggroups [[:ground :humanshape]]
  :abilities [[:blaze :iron-fist]]
  :stats [[76 104 71 104 71 108]]
  :weight [55]
  :height [1.2])

(deffamily (familychain :chimchar :monferno :infernape))

(deflearnset :dp
  :chimchar [:aerial-ace :assist :attract :blaze-kick :brick-break :bulk-up :captivate :counter :cut :dig :double-kick :double-team :ember :encore :endeavor :endure :facade :fake-out :fire-blast :fire-punch :fire-spin :flame-wheel :flamethrower :fling :focus-energy :focus-punch :frustration :fury-swipes :grass-knot :gunk-shot :headbutt :heat-wave :helping-hand :hidden-power :iron-tail :leer :low-kick :mud-slap :nasty-plot :natural-gift :overheat :protect :rest :return :rock-climb :rock-smash :role-play :rollout :scratch :secret-power :shadow-claw :slack-off :sleep-talk :snore :stealth-rock :strength :substitute :sunny-day :swagger :swift :swords-dance :taunt :thunderpunch :torment :toxic :u-turn :uproar :vacuum-wave :will-o-wisp]
  :monferno [:close-combat :feint :flare-blitz :focus-blast :mach-punch :poison-jab :rock-slide :rock-tomb]
  :infernape [:blast-burn :calm-mind :earthquake :giga-impact :hyper-beam :punishment :roar :solarbeam :stone-edge])

(deflearnset :bw
  :chimchar [:acrobatics :aerial-ace :assist :attract :blaze-kick :brick-break :bulk-up :counter :covet :cut :dig :double-kick :double-team :ember :encore :endeavor :facade :fake-out :fire-blast :fire-pledge :fire-punch :fire-spin :flame-charge :flame-wheel :flamethrower :fling :focus-energy :focus-punch :frustration :fury-swipes :grass-knot :gunk-shot :heat-wave :helping-hand :hidden-power :hone-claws :incinerate :iron-tail :leer :low-kick :low-sweep :nasty-plot :overheat :protect :quick-guard :rest :return :rock-smash :role-play :round :scratch :shadow-claw :slack-off :sleep-talk :snore :stealth-rock :strength :submission :substitute :sunny-day :swagger :swords-dance :taunt :thunderpunch :torment :toxic :u-turn :uproar :will-o-wisp]
  :monferno [:close-combat :dual-chop :feint :flare-blitz :focus-blast :mach-punch :poison-jab :retaliate :rock-slide :rock-tomb :work-up]
  :infernape [:blast-burn :bulldoze :calm-mind :earthquake :giga-impact :hyper-beam :punishment :roar :solarbeam :stone-edge])
