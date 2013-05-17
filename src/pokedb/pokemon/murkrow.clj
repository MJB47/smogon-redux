
(in-ns 'smogon.dex)


(defpokemon :murkrow
  :name "Murkrow"
  :introduced-in :gs
  :types [[:dark :flying]]
  :egggroups [[:flying]]
  :abilities [[:insomnia]
              :dp [:insomnia :super-luck]
              :bw [:insomnia :prankster :super-luck]]
  :stats [[60 85 42 85 42 91]]
  :weight [2.1]
  :height [0.5])

(defpokemon :honchkrow
  :name "Honchkrow"
  :introduced-in :dp
  :types [[:dark :flying]]
  :egggroups [[:flying]]
  :abilities [[:insomnia :moxie :super-luck]]
  :stats [[100 125 52 105 52 71]]
  :weight [27.3]
  :height [0.9])

(deffamily (familychain :murkrow :honchkrow))

(deflearnset :gs
  :murkrow [:attract :curse :detect :double-team :dream-eater :drill-peck :endure :faint-attack :fly :frustration :haze :hidden-power :icy-wind :mean-look :mirror-move :mud-slap :night-shade :nightmare :peck :protect :psych-up :pursuit :quick-attack :rest :return :shadow-ball :sky-attack :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :thief :toxic :whirlwind :wing-attack])

(deflearnset :rs
  :murkrow [:aerial-ace :astonish :attract :calm-mind :confuse-ray :double-edge :double-team :dream-eater :drill-peck :endure :facade :faint-attack :featherdance :fly :frustration :haze :hidden-power :icy-wind :mean-look :mimic :mirror-move :mud-slap :night-shade :nightmare :peck :perish-song :protect :psych-up :pursuit :rain-dance :rest :return :secret-power :shadow-ball :sky-attack :sleep-talk :snatch :snore :steel-wing :substitute :sunny-day :swagger :swift :taunt :thief :thunder-wave :torment :toxic :whirlwind :wing-attack :double-edge :icy-wind :mimic :nightmare])

(deflearnset :dp
  :murkrow [:aerial-ace :air-cutter :assurance :astonish :attract :brave-bird :calm-mind :captivate :confuse-ray :dark-pulse :defog :double-team :dream-eater :drill-peck :embargo :endure :facade :faint-attack :featherdance :fly :frustration :haze :heat-wave :hidden-power :icy-wind :mean-look :mirror-move :mud-slap :natural-gift :night-shade :ominous-wind :payback :peck :perish-song :pluck :protect :psych-up :psychic :psycho-shift :pursuit :rain-dance :rest :return :roost :screech :secret-power :shadow-ball :sky-attack :sleep-talk :snatch :spite :steel-wing :substitute :sucker-punch :sunny-day :swagger :swift :tailwind :taunt :thief :thunder-wave :torment :toxic :twister :uproar :whirlwind :wing-attack :double-edge :icy-wind :mimic :nightmare]
  :honchkrow [:giga-impact :hyper-beam :nasty-plot :night-slash :superpower])

(deflearnset :bw
  :murkrow [:aerial-ace :assurance :astonish :attract :brave-bird :calm-mind :confuse-ray :dark-pulse :double-team :dream-eater :drill-peck :embargo :facade :faint-attack :featherdance :fly :foul-play :frustration :haze :heat-wave :hidden-power :icy-wind :mean-look :mirror-move :night-shade :payback :peck :perish-song :pluck :protect :psych-up :psychic :psycho-shift :pursuit :quash :rain-dance :rest :retaliate :return :roost :round :screech :shadow-ball :sky-attack :sleep-talk :snarl :snatch :spite :substitute :sucker-punch :sunny-day :swagger :tailwind :taunt :thief :thunder-wave :torment :toxic :uproar :whirlwind :wing-attack :double-edge :icy-wind :mimic :nightmare]
  :honchkrow [:giga-impact :hyper-beam :incinerate :nasty-plot :night-slash :superpower])
