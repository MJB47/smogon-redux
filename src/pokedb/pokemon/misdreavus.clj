
(in-ns 'smogon.dex)


(defpokemon :misdreavus
  :name "Misdreavus"
  :introduced-in :gs
  :types [[:ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]
              :bw [:levitate]]
  :stats [[60 60 60 85 85 85]]
  :weight [1]
  :height [0.7])

(defpokemon :mismagius
  :name "Mismagius"
  :introduced-in :dp
  :types [[:ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[60 60 60 105 105 105]]
  :weight [4.4]
  :height [0.9])

(deffamily (familychain :misdreavus :mismagius))

(deflearnset :gs
  :misdreavus [:attract :confuse-ray :curse :defense-curl :destiny-bond :double-team :dream-eater :endure :flash :frustration :growl :headbutt :hidden-power :mean-look :nightmare :pain-split :perish-song :protect :psybeam :psych-up :psychic :psywave :rain-dance :rest :return :screech :shadow-ball :sleep-talk :snore :spite :sunny-day :swagger :swift :thief :thunder :thunderbolt :toxic :zap-cannon])

(deflearnset :rs
  :misdreavus [:aerial-ace :astonish :attract :calm-mind :confuse-ray :defense-curl :destiny-bond :double-edge :double-team :dream-eater :endure :facade :flash :frustration :growl :grudge :hidden-power :imprison :mean-look :mimic :nightmare :pain-split :perish-song :protect :psybeam :psych-up :psychic :psywave :rain-dance :rest :return :screech :secret-power :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :spite :substitute :sunny-day :swagger :swift :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :double-edge :dream-eater :mimic :nightmare])

(deflearnset :dp
  :misdreavus [:aerial-ace :astonish :attract :calm-mind :captivate :charge-beam :confuse-ray :curse :dark-pulse :destiny-bond :double-team :dream-eater :embargo :endure :facade :flash :frustration :growl :grudge :headbutt :heal-bell :hidden-power :icy-wind :imprison :magic-coat :mean-look :memento :nasty-plot :natural-gift :ominous-wind :pain-split :payback :perish-song :power-gem :protect :psybeam :psych-up :psychic :psywave :rain-dance :rest :return :screech :secret-power :shadow-ball :shadow-sneak :shock-wave :skill-swap :sleep-talk :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :swift :taunt :thief :thunder :thunder-wave :thunderbolt :torment :toxic :trick :trick-room :uproar :will-o-wisp :double-edge :dream-eater :mimic :nightmare]
  :mismagius [:energy-ball :giga-impact :hyper-beam :lucky-chant :magical-leaf])

(deflearnset :bw
  :misdreavus [:aerial-ace :astonish :attract :calm-mind :charge-beam :confuse-ray :curse :dark-pulse :destiny-bond :double-team :dream-eater :echoed-voice :embargo :facade :flash :foul-play :frustration :growl :grudge :heal-bell :hex :hidden-power :hyper-voice :icy-wind :imprison :magic-coat :magic-room :mean-look :memento :nasty-plot :ominous-wind :pain-split :payback :perish-song :power-gem :protect :psybeam :psych-up :psychic :psywave :rain-dance :rest :return :round :screech :shadow-ball :shadow-sneak :skill-swap :sleep-talk :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :taunt :telekinesis :thief :thunder :thunder-wave :thunderbolt :torment :toxic :trick :trick-room :uproar :will-o-wisp :wonder-room :double-edge :dream-eater :mimic :nightmare]
  :mismagius [:energy-ball :giga-impact :hyper-beam :lucky-chant :magical-leaf])
