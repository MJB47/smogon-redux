
(in-ns 'smogon.dex)


(defpokemon :mime-jr
  :name "Mime Jr."
  :introduced-in :dp
  :types [[:psychic]]
  :egggroups [[]]
  :abilities [[:filter :soundproof :technician]]
  :stats [[20 25 45 70 90 60]]
  :weight [13]
  :height [0.6])

(defpokemon :mr-mime
  :name "Mr. Mime"
  :introduced-in :rb
  :types [[:psychic]]
  :egggroups [[:humanshape]]
  :abilities [[:soundproof]
              :dp [:filter :soundproof]
              :bw [:filter :soundproof :technician]]
  :stats [[40 45 65 100 90]
          :gs [40 45 65 100 120 90]]
  :weight [54.5]
  :height [1.3])

(deffamily (familychain :mime-jr :mr-mime))

(deflearnset :rb
  :mr-mime [:barrier :bide :body-slam :confusion :counter :double-edge :double-team :doubleslap :flash :hyper-beam :light-screen :meditate :mega-kick :mega-punch :metronome :mimic :psychic :psywave :rage :reflect :rest :seismic-toss :skull-bash :solarbeam :submission :substitute :take-down :teleport :thunder :thunder-wave :thunderbolt :toxic])

(deflearnset :gs
  :mr-mime [:attract :barrier :baton-pass :confusion :curse :double-team :doubleslap :dream-eater :dynamicpunch :encore :endure :fire-punch :flash :frustration :future-sight :headbutt :hidden-power :hyper-beam :hypnosis :ice-punch :light-screen :meditate :mimic :mud-slap :nightmare :protect :psybeam :psych-up :psychic :reflect :rest :return :safeguard :shadow-ball :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :thief :thunder :thunderbolt :thunderpunch :toxic :zap-cannon])

(deflearnset :rs
  :mr-mime [:attract :barrier :baton-pass :body-slam :brick-break :calm-mind :confusion :counter :double-edge :double-team :doubleslap :dream-eater :encore :endure :facade :fake-out :fire-punch :flash :focus-punch :frustration :future-sight :hidden-power :hyper-beam :hypnosis :ice-punch :light-screen :magical-leaf :meditate :mega-kick :mega-punch :metronome :mimic :mud-slap :nightmare :protect :psybeam :psych-up :psychic :rain-dance :recycle :reflect :rest :return :role-play :safeguard :secret-power :seismic-toss :shadow-ball :shock-wave :skill-swap :sleep-talk :snatch :snore :solarbeam :substitute :sunny-day :swagger :taunt :thief :thunder :thunder-wave :thunderbolt :thunderpunch :torment :toxic :trick :body-slam :double-edge :follow-me :nightmare :seismic-toss :thunderpunch])

(deflearnset :dp
  :mime-jr [:attract :barrier :baton-pass :brick-break :calm-mind :captivate :charge-beam :charm :confuse-ray :confusion :copycat :double-team :doubleslap :drain-punch :dream-eater :encore :endure :facade :fake-out :flash :fling :focus-punch :frustration :future-sight :grass-knot :headbutt :healing-wish :helping-hand :hidden-power :hypnosis :light-screen :magic-coat :meditate :mimic :mud-slap :nasty-plot :natural-gift :protect :psybeam :psych-up :psychic :rain-dance :recycle :reflect :rest :return :role-play :safeguard :secret-power :shadow-ball :shock-wave :signal-beam :skill-swap :sleep-talk :snatch :snore :solarbeam :substitute :sunny-day :swagger :taunt :teeter-dance :thief :thunder :thunder-wave :thunderbolt :tickle :torment :toxic :trick :trick-room :uproar :wake-up-slap]
  :mr-mime [:aerial-ace :energy-ball :fire-punch :focus-blast :giga-impact :guard-swap :hyper-beam :ice-punch :iron-defense :magical-leaf :payback :power-swap :thunderpunch :zen-headbutt :body-slam :double-edge :follow-me :nightmare :seismic-toss :thunderpunch])

(deflearnset :bw
  :mime-jr [:attract :barrier :baton-pass :brick-break :calm-mind :charge-beam :charm :confuse-ray :confusion :copycat :covet :double-team :doubleslap :drain-punch :dream-eater :encore :facade :fake-out :flash :fling :frustration :future-sight :grass-knot :healing-wish :helping-hand :hidden-power :hypnosis :icy-wind :light-screen :magic-coat :magic-room :meditate :mimic :nasty-plot :power-split :protect :psybeam :psych-up :psychic :psyshock :rain-dance :recycle :reflect :rest :return :role-play :round :safeguard :shadow-ball :signal-beam :skill-swap :sleep-talk :snatch :snore :solarbeam :substitute :sunny-day :swagger :taunt :teeter-dance :telekinesis :thief :thunder :thunder-wave :thunderbolt :tickle :torment :toxic :trick :trick-room :uproar :wake-up-slap :wonder-room]
  :mr-mime [:aerial-ace :energy-ball :fire-punch :focus-blast :foul-play :giga-impact :guard-swap :hyper-beam :ice-punch :iron-defense :magical-leaf :payback :power-swap :psywave :quick-guard :thunderpunch :wide-guard :zen-headbutt :body-slam :double-edge :follow-me :nightmare :seismic-toss :thunderpunch])
