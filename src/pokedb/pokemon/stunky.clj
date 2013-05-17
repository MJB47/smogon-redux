
(in-ns 'smogon.dex)


(defpokemon :stunky
  :name "Stunky"
  :introduced-in :dp
  :types [[:dark :poison]]
  :egggroups [[:ground]]
  :abilities [[:aftermath :keen-eye :stench]]
  :stats [[63 63 47 41 41 74]]
  :weight [19.2]
  :height [0.4])

(defpokemon :skuntank
  :name "Skuntank"
  :introduced-in :dp
  :types [[:dark :poison]]
  :egggroups [[:ground]]
  :abilities [[:aftermath :keen-eye :stench]]
  :stats [[103 93 67 71 61 84]]
  :weight [38]
  :height [1])

(deffamily (familychain :stunky :skuntank))



(deflearnset :dp
  :stunky [:astonish :attract :captivate :crunch :cut :dark-pulse :defog :dig :double-edge :double-team :endure :explosion :facade :feint :fire-blast :flamethrower :focus-energy :frustration :fury-cutter :fury-swipes :haze :headbutt :hidden-power :iron-tail :leer :memento :mud-slap :natural-gift :night-slash :payback :poison-gas :protect :punishment :pursuit :rain-dance :rest :return :roar :rock-smash :scary-face :scratch :screech :secret-power :shadow-ball :shadow-claw :slash :sleep-talk :sludge-bomb :smog :smokescreen :snatch :snore :substitute :sucker-punch :sunny-day :swagger :swift :taunt :thief :torment :toxic]
  :skuntank [:giga-impact :hyper-beam :poison-jab :strength])

(deflearnset :bw
  :stunky [:acid-spray :astonish :attract :crunch :cut :dark-pulse :dig :double-edge :double-team :explosion :facade :feint :fire-blast :flame-burst :flamethrower :focus-energy :foul-play :frustration :fury-swipes :haze :hidden-power :hone-claws :incinerate :iron-tail :leer :memento :night-slash :payback :poison-gas :protect :punishment :pursuit :rain-dance :rest :return :roar :rock-smash :round :scary-face :scratch :screech :shadow-ball :shadow-claw :slash :sleep-talk :sludge-bomb :smog :smokescreen :snarl :snatch :snore :substitute :sunny-day :swagger :taunt :thief :torment :toxic :venoshock]
  :skuntank [:giga-impact :hyper-beam :poison-jab :strength])

