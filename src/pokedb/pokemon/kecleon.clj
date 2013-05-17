
(in-ns 'smogon.dex)


(defpokemon :kecleon
  :name "Kecleon"
  :introduced-in :rs
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:color-change]
              :bw [:color-change]]
  :stats [[60 90 70 60 120 40]]
  :weight [22]
  :height [1])

(deffamily (familychain :kecleon))

(deflearnset :rs
  :kecleon [:aerial-ace :ancientpower :astonish :attract :bind :blizzard :body-slam :brick-break :counter :cut :defense-curl :dig :disable :double-edge :double-team :dynamicpunch :endure :facade :faint-attack :fire-blast :fire-punch :flamethrower :flash :focus-punch :frustration :fury-cutter :fury-swipes :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :lick :magic-coat :mega-kick :mega-punch :metronome :mimic :mud-slap :protect :psybeam :psych-up :rain-dance :rest :return :rock-slide :rock-smash :rock-tomb :rollout :scratch :screech :secret-power :seismic-toss :shadow-ball :shock-wave :skill-swap :slash :sleep-talk :snatch :snore :solarbeam :strength :substitute :sunny-day :swagger :swift :tail-whip :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :water-pulse :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :dp
  :kecleon [:aerial-ace :ancientpower :aqua-tail :astonish :attract :bind :blizzard :brick-break :captivate :charge-beam :cut :dig :disable :dizzy-punch :double-team :drain-punch :endure :facade :faint-attack :fake-out :feint :fire-blast :fire-punch :flamethrower :flash :fling :focus-punch :frustration :fury-cutter :fury-swipes :grass-knot :headbutt :hidden-power :ice-beam :ice-punch :icy-wind :iron-tail :knock-off :last-resort :lick :low-kick :magic-coat :mud-slap :nasty-plot :natural-gift :protect :psybeam :psych-up :rain-dance :recover :recycle :rest :return :rock-slide :rock-smash :rock-tomb :role-play :rollout :scratch :screech :secret-power :shadow-ball :shadow-claw :shadow-sneak :shock-wave :skill-swap :slash :sleep-talk :snatch :snore :solarbeam :stealth-rock :strength :substitute :sucker-punch :sunny-day :swagger :swift :tail-whip :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :water-pulse :body-slam :double-edge :icy-wind :mimic :seismic-toss])

(deflearnset :bw
  :kecleon [:aerial-ace :after-you :ancientpower :aqua-tail :astonish :attract :bind :blizzard :brick-break :charge-beam :cut :dig :disable :dizzy-punch :double-team :drain-punch :facade :faint-attack :fake-out :feint :fire-blast :fire-punch :flamethrower :flash :fling :foul-play :frustration :fury-swipes :grass-knot :hidden-power :hone-claws :ice-beam :ice-punch :icy-wind :incinerate :iron-tail :knock-off :last-resort :lick :low-kick :magic-coat :nasty-plot :protect :psybeam :psych-up :rain-dance :recover :recycle :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :role-play :round :scratch :screech :shadow-ball :shadow-claw :shadow-sneak :skill-swap :slash :sleep-talk :snatch :snore :solarbeam :stealth-rock :strength :substitute :sucker-punch :sunny-day :swagger :synchronoise :tail-whip :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :trick-room :wonder-room :work-up :body-slam :double-edge :icy-wind :mimic :seismic-toss])
