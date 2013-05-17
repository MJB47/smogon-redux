
(in-ns 'smogon.dex)


(defpokemon :sableye
  :name "Sableye"
  :introduced-in :rs
  :types [[:dark :ghost]]
  :egggroups [[:humanshape]]
  :abilities [[:keen-eye]
              :dp [:keen-eye :stall]
              :bw [:keen-eye :prankster :stall]]
  :stats [[50 75 75 65 65 50]]
  :weight [11]
  :height [0.5])

(deffamily (familychain :sableye))

(deflearnset :rs
  :sableye [:aerial-ace :astonish :attract :body-slam :brick-break :calm-mind :confuse-ray :counter :cut :detect :dig :double-edge :double-team :dream-eater :dynamicpunch :endure :facade :faint-attack :fake-out :fire-punch :flash :focus-punch :foresight :frustration :fury-cutter :fury-swipes :hidden-power :ice-punch :knock-off :leer :mean-look :mega-kick :mega-punch :metronome :mimic :moonlight :mud-slap :night-shade :nightmare :protect :psych-up :psychic :rain-dance :recover :rest :return :rock-smash :rock-tomb :scratch :secret-power :seismic-toss :shadow-ball :shock-wave :sleep-talk :snatch :snore :substitute :sunny-day :swagger :taunt :thief :thunderpunch :torment :toxic :water-pulse :body-slam :double-edge :helping-hand :mimic :nightmare :seismic-toss :substitute])

(deflearnset :dp
  :sableye [:aerial-ace :astonish :attract :brick-break :calm-mind :captivate :confuse-ray :cut :dark-pulse :detect :dig :double-team :dream-eater :embargo :endure :facade :faint-attack :fake-out :feint :fire-punch :flash :flatter :fling :focus-punch :foresight :frustration :fury-cutter :fury-swipes :gravity :headbutt :hidden-power :ice-punch :icy-wind :knock-off :leer :low-kick :magic-coat :mean-look :moonlight :mud-slap :nasty-plot :natural-gift :night-shade :ominous-wind :pain-split :payback :poison-jab :power-gem :protect :psych-up :psychic :punishment :rain-dance :recover :rest :return :rock-smash :rock-tomb :role-play :scratch :secret-power :shadow-ball :shadow-claw :shadow-sneak :shock-wave :signal-beam :sleep-talk :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :thief :thunderpunch :torment :toxic :trick :water-pulse :will-o-wisp :zen-headbutt :body-slam :double-edge :helping-hand :mimic :nightmare :seismic-toss :substitute])

(deflearnset :bw
  :sableye [:aerial-ace :astonish :attract :brick-break :calm-mind :captivate :confuse-ray :cut :dark-pulse :detect :dig :double-team :dream-eater :embargo :facade :faint-attack :fake-out :feint :fire-punch :flash :flatter :fling :foresight :foul-play :frustration :fury-swipes :gravity :hidden-power :hone-claws :ice-punch :icy-wind :incinerate :knock-off :leer :low-kick :low-sweep :magic-coat :mean-look :metal-burst :moonlight :nasty-plot :night-shade :payback :poison-jab :power-gem :protect :psych-up :psychic :punishment :rain-dance :recover :rest :retaliate :return :rock-smash :rock-tomb :role-play :round :scratch :shadow-ball :shadow-claw :shadow-sneak :signal-beam :sleep-talk :snarl :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :telekinesis :thief :thunderpunch :torment :toxic :trick :will-o-wisp :wonder-room :zen-headbutt :body-slam :double-edge :helping-hand :mimic :nightmare :seismic-toss :substitute])
