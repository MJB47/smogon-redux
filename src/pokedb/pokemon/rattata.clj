
(in-ns 'smogon.dex)


(defpokemon :rattata
  :name "Rattata"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:guts :run-away]
              :bw [:guts :hustle :run-away]]
  :stats [[30 56 35 25 72]
          :gs [30 56 35 25 35 72]]
  :weight [3.5]
  :height [0.3])

(defpokemon :raticate
  :name "Raticate"
  :introduced-in :rb
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:guts :run-away]
              :bw [:guts :hustle :run-away]]
  :stats [[55 81 60 50 97]
          :gs [55 81 60 50 70 97]]
  :weight [18.5]
  :height [0.7])

(deffamily (familychain :rattata :raticate))

(deflearnset :rb
  :rattata [:bide :blizzard :body-slam :bubblebeam :dig :double-edge :double-team :focus-energy :hyper-fang :mimic :quick-attack :rage :rest :skull-bash :substitute :super-fang :swift :tackle :tail-whip :take-down :thunder :thunderbolt :toxic :water-gun]
  :raticate [:hyper-beam :ice-beam])

(deflearnset :gs
  :rattata [:attract :bite :blizzard :counter :curse :defense-curl :dig :double-team :endure :flame-wheel :focus-energy :frustration :fury-swipes :headbutt :hidden-power :hyper-fang :icy-wind :iron-tail :mud-slap :protect :pursuit :quick-attack :rest :return :reversal :rock-smash :screech :shadow-ball :sleep-talk :snore :sunny-day :super-fang :swagger :swift :tackle :tail-whip :thief :thunder :toxic]
  :raticate [:cut :hyper-beam :ice-beam :roar :scary-face :strength :thunderbolt])

(deflearnset :rs
  :rattata [:attract :bite :blizzard :body-slam :counter :cut :defense-curl :dig :double-edge :double-team :endeavor :endure :facade :flame-wheel :focus-energy :frustration :fury-swipes :hidden-power :hyper-fang :ice-beam :icy-wind :iron-tail :mimic :mud-slap :protect :pursuit :quick-attack :rain-dance :rest :return :reversal :rock-smash :screech :secret-power :shadow-ball :shock-wave :sleep-talk :snore :substitute :sunny-day :super-fang :swagger :swift :tackle :tail-whip :taunt :thief :thunder :thunder-wave :thunderbolt :toxic :uproar :body-slam :icy-wind :mimic]
  :raticate [:hyper-beam :roar :scary-face :strength :body-slam :icy-wind :mimic :refresh])

(deflearnset :dp
  :rattata [:assurance :attract :bite :blizzard :captivate :charge-beam :counter :crunch :cut :dig :double-edge :double-team :endeavor :endure :facade :flame-wheel :focus-energy :frustration :fury-swipes :grass-knot :headbutt :hidden-power :hyper-fang :ice-beam :icy-wind :iron-tail :last-resort :me-first :mud-slap :natural-gift :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :reversal :rock-smash :screech :secret-power :shadow-ball :shock-wave :sleep-talk :snore :substitute :sucker-punch :sunny-day :super-fang :swagger :swift :tackle :tail-whip :taunt :thief :thunder :thunder-wave :thunderbolt :toxic :u-turn :uproar :zen-headbutt :body-slam :icy-wind :mimic]
  :raticate [:giga-impact :hyper-beam :roar :scary-face :strength :swords-dance :body-slam :icy-wind :mimic :refresh])

(deflearnset :bw
  :rattata [:assurance :attract :bite :blizzard :charge-beam :counter :covet :crunch :cut :dig :double-edge :double-team :endeavor :facade :final-gambit :flame-wheel :focus-energy :frustration :fury-swipes :grass-knot :hidden-power :hyper-fang :ice-beam :icy-wind :iron-tail :last-resort :me-first :pluck :protect :pursuit :quick-attack :rain-dance :rest :retaliate :return :revenge :reversal :rock-smash :round :screech :shadow-ball :sleep-talk :snore :substitute :sucker-punch :sunny-day :super-fang :swagger :tackle :tail-whip :taunt :thief :thunder :thunder-wave :thunderbolt :toxic :u-turn :uproar :wild-charge :work-up :zen-headbutt :body-slam :icy-wind :mimic]
  :raticate [:giga-impact :hyper-beam :roar :scary-face :strength :swords-dance :body-slam :icy-wind :mimic :refresh])
