
(in-ns 'smogon.dex)


(defpokemon :cubchoo
  :name "Cubchoo"
  :introduced-in :bw
  :types [[:ice]]
  :egggroups [[:ground]]
  :abilities [[:rattled :snow-cloak]]
  :stats [[55 70 40 60 40 40]]
  :weight [8.5]
  :height [0.5])

(defpokemon :beartic
  :name "Beartic"
  :introduced-in :bw
  :types [[:ice]]
  :egggroups [[:ground]]
  :abilities [[:snow-cloak :swift-swim]]
  :stats [[95 110 80 70 80 50]]
  :weight [260]
  :height [2.6])

(deffamily (familychain :cubchoo :beartic))

(deflearnset :bw
  :cubchoo [:aerial-ace :assurance :attract :avalanche :bide :blizzard :brine :charm :covet :cut :dig :double-team :echoed-voice :encore :endure :facade :flail :fling :focus-punch :frost-breath :frustration :fury-swipes :grass-knot :growl :hail :hidden-power :hone-claws :ice-beam :ice-punch :icy-wind :low-kick :night-slash :powder-snow :protect :rain-dance :rest :return :rock-smash :rock-tomb :round :shadow-claw :sheer-cold :slash :sleep-talk :snore :strength :substitute :superpower :surf :swagger :thrash :toxic :yawn]
  :beartic [:aqua-jet :brick-break :bulk-up :bulldoze :dive :focus-blast :giga-impact :hyper-beam :icicle-crash :roar :rock-slide :stone-edge :swords-dance :taunt])
