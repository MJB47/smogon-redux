
(in-ns 'smogon.dex)

(defpokemon :genesect
  :name "Genesect"
  :introduced-in :bw
  :types [[:bug :steel]]
  :egggroups [[]]
  :abilities [[:download]]
  :stats [[71 120 95 120 95 99]]
  :weight [82.5]
  :height [1.5])

(deffamily (familychain :genesect))

(deflearnset :bw
  :genesect [:aerial-ace :blizzard :bug-bite :bug-buzz :charge-beam :dark-pulse :double-team :electroweb :energy-ball :explosion :facade :flame-charge :flamethrower :flash :flash-cannon :fly :frustration :fury-cutter :giga-drain :giga-impact :gravity :gunk-shot :hidden-power :hone-claws :hyper-beam :ice-beam :iron-defense :iron-head :last-resort :light-screen :lock-on :magic-coat :magnet-bomb :magnet-rise :metal-claw :metal-sound :protect :psychic :quick-attack :recycle :reflect :rest :return :rock-polish :round :screech :selfdestruct :shadow-claw :signal-beam :simple-beam :slash :sleep-talk :snore :solarbeam :signal-beam :struggle-bug :substitute :swagger :techno-blast :thunder :thunder-wave :thunderbolt :toxic :tri-attack :u-turn :x-scissor :zap-cannon :zen-headbutt])
