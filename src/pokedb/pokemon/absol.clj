
(in-ns 'smogon.dex)

(defpokemon :absol
  :name "Absol"
  :introduced-in :rs
  :types [[:dark]]
  :egggroups [[:ground]]
  :abilities [[:pressure]
              :dp [:pressure :super-luck]
              :bw [:justified :pressure :super-luck]]
  :stats [[65 130 60 75 60 75]]
  :weight [47]
  :height [1.2])

(deffamily (familychain :absol))

(deflearnset :rs
  :absol [:aerial-ace :attract :baton-pass :bite :blizzard :body-slam :calm-mind :counter :curse :cut :double-edge :double-team :dream-eater :endure :facade :faint-attack :fire-blast :flamethrower :flash :frustration :fury-cutter :future-sight :hail :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :leer :magic-coat :mimic :mud-slap :nightmare :perish-song :protect :psych-up :quick-attack :rain-dance :razor-wind :rest :return :rock-slide :rock-smash :sandstorm :scratch :secret-power :shadow-ball :shock-wave :slash :sleep-talk :snatch :snore :strength :substitute :sunny-day :swagger :swift :swords-dance :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :water-pulse])

(deflearnset :dp
  :absol [:aerial-ace :assurance :attract :baton-pass :bite :blizzard :bounce :calm-mind :captivate :charge-beam :curse :cut :dark-pulse :detect :double-edge :double-team :dream-eater :endure :facade :faint-attack :false-swipe :feint :fire-blast :flamethrower :flash :frustration :fury-cutter :future-sight :giga-impact :hail :headbutt :hidden-power :hyper-beam :ice-beam :icy-wind :iron-tail :knock-off :leer :magic-coat :me-first :mean-look :megahorn :mud-slap :natural-gift :night-slash :nightmare :payback :perish-song :protect :psych-up :psycho-cut :punishment :pursuit :quick-attack :rain-dance :razor-wind :rest :return :rock-slide :rock-smash :rock-tomb :role-play :sandstorm :scratch :secret-power :shadow-ball :shadow-claw :shock-wave :slash :sleep-talk :snatch :snore :spite :stone-edge :strength :substitute :sucker-punch :sunny-day :superpower :swagger :swift :swords-dance :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :water-pulse :will-o-wisp :x-scissor :zen-headbutt])

(deflearnset :bw
  :absol [:aerial-ace :assurance :attract :baton-pass :bite :blizzard :bounce :calm-mind :charge-beam :curse :cut :dark-pulse :detect :double-edge :double-team :dream-eater :echoed-voice :facade :faint-attack :false-swipe :feint :fire-blast :flamethrower :flash :foul-play :frustration :future-sight :giga-impact :hail :hex :hidden-power :hone-claws :hyper-beam :ice-beam :icy-wind :incinerate :iron-tail :knock-off :leer :magic-coat :me-first :mean-look :megahorn :night-slash :nightmare :payback :perish-song :protect :psych-up :psycho-cut :punishment :pursuit :quick-attack :rain-dance :razor-wind :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :role-play :round :sandstorm :scratch :shadow-ball :shadow-claw :slash :sleep-talk :snarl :snatch :snore :spite :stone-edge :strength :substitute :sucker-punch :sunny-day :superpower :swagger :swords-dance :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :will-o-wisp :x-scissor :zen-headbutt])
