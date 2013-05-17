
(in-ns 'smogon.dex)


(defpokemon :hoothoot
  :name "Hoothoot"
  :introduced-in :gs
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:insomnia :keen-eye]
              :bw [:insomnia :keen-eye :tinted-lens]]
  :stats [[60 30 30 36 56 50]]
  :weight [21.2]
  :height [0.7])

(defpokemon :noctowl
  :name "Noctowl"
  :introduced-in :gs
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:insomnia :keen-eye]
              :bw [:insomnia :keen-eye :tinted-lens]]
  :stats [[100 50 50 76 96 70]]
  :weight [40.8]
  :height [1.6])

(deffamily (familychain :hoothoot :noctowl))

(deflearnset :gs
  :hoothoot [:attract :confusion :curse :detect :double-team :dream-eater :endure :faint-attack :flash :fly :foresight :frustration :growl :hidden-power :hypnosis :mirror-move :mud-slap :nightmare :peck :protect :reflect :rest :return :sky-attack :sleep-talk :snore :steel-wing :sunny-day :supersonic :swagger :swift :tackle :take-down :thief :toxic :whirlwind :wing-attack]
  :noctowl [:hyper-beam])

(deflearnset :rs
  :hoothoot [:aerial-ace :attract :confusion :double-edge :double-team :dream-eater :endure :facade :faint-attack :featherdance :flash :fly :foresight :frustration :growl :hidden-power :hypnosis :mimic :mirror-move :mud-slap :nightmare :peck :protect :psychic :rain-dance :reflect :rest :return :secret-power :shadow-ball :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :swift :tackle :take-down :thief :toxic :whirlwind :wing-attack]
  :noctowl [:hyper-beam])

(deflearnset :dp
  :hoothoot [:aerial-ace :agility :air-cutter :air-slash :attract :captivate :confusion :defog :double-team :dream-eater :endure :extrasensory :facade :faint-attack :featherdance :fly :foresight :frustration :growl :heat-wave :hidden-power :hypnosis :magic-coat :mirror-move :mud-slap :natural-gift :night-shade :ominous-wind :peck :pluck :protect :psych-up :psychic :psycho-shift :rain-dance :recycle :reflect :rest :return :roost :secret-power :shadow-ball :silver-wind :sky-attack :sleep-talk :steel-wing :substitute :sunny-day :supersonic :swagger :swift :tackle :tailwind :take-down :thief :toxic :twister :uproar :whirlwind :wing-attack :zen-headbutt]
  :noctowl [:giga-impact :hyper-beam])

(deflearnset :bw
  :hoothoot [:aerial-ace :agility :air-slash :attract :confusion :defog :double-team :dream-eater :echoed-voice :extrasensory :facade :faint-attack :featherdance :fly :foresight :frustration :growl :heat-wave :hidden-power :hyper-voice :hypnosis :magic-coat :mirror-move :night-shade :peck :pluck :protect :psych-up :psychic :psycho-shift :rain-dance :recycle :reflect :rest :return :roost :round :shadow-ball :sky-attack :sleep-talk :substitute :sunny-day :supersonic :swagger :synchronoise :tackle :tailwind :take-down :thief :toxic :uproar :whirlwind :wing-attack :work-up :zen-headbutt]
  :noctowl [:giga-impact :hyper-beam])
