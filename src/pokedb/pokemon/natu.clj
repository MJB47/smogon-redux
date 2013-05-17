
(in-ns 'smogon.dex)


(defpokemon :natu
  :name "Natu"
  :introduced-in :gs
  :types [[:flying :psychic]]
  :egggroups [[:flying]]
  :abilities [[:early-bird :synchronize]
              :bw [:early-bird :magic-bounce :synchronize]]
  :stats [[40 50 45 70 45 70]]
  :weight [2]
  :height [0.2])

(defpokemon :xatu
  :name "Xatu"
  :introduced-in :gs
  :types [[:flying :psychic]]
  :egggroups [[:flying]]
  :abilities [[:early-bird :synchronize]
              :bw [:early-bird :magic-bounce :synchronize]]
  :stats [[65 75 70 95 70 95]]
  :weight [15]
  :height [1.5])

(deffamily (familychain :natu :xatu))

(deflearnset :gs
  :natu [:attract :confuse-ray :curse :detect :double-team :dream-eater :drill-peck :endure :faint-attack :flash :frustration :future-sight :giga-drain :haze :hidden-power :leer :night-shade :nightmare :peck :protect :psych-up :psychic :quick-attack :rest :return :sleep-talk :snore :solarbeam :steel-wing :sunny-day :swagger :swift :teleport :thief :toxic]
  :xatu [:fly :hyper-beam])

(deflearnset :rs
  :natu [:aerial-ace :attract :calm-mind :confuse-ray :double-edge :double-team :dream-eater :drill-peck :endure :facade :faint-attack :featherdance :flash :frustration :future-sight :giga-drain :haze :hidden-power :leer :light-screen :mimic :night-shade :nightmare :peck :protect :psych-up :psychic :quick-attack :rain-dance :reflect :refresh :rest :return :secret-power :shadow-ball :skill-swap :sky-attack :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :swagger :swift :teleport :thief :thunder-wave :toxic :wish :baton-pass]
  :xatu [:fly :hyper-beam])

(deflearnset :dp
  :natu [:aerial-ace :air-cutter :attract :calm-mind :captivate :confuse-ray :double-team :dream-eater :drill-peck :endure :facade :faint-attack :featherdance :flash :frustration :future-sight :giga-drain :grass-knot :guard-swap :haze :heat-wave :hidden-power :leer :light-screen :lucky-chant :magic-coat :me-first :miracle-eye :natural-gift :night-shade :ominous-wind :pain-split :peck :pluck :power-swap :protect :psych-up :psychic :psycho-shift :quick-attack :rain-dance :reflect :refresh :rest :return :roost :secret-power :shadow-ball :signal-beam :silver-wind :skill-swap :sleep-talk :snore :solarbeam :steel-wing :substitute :sucker-punch :sunny-day :swagger :swift :tailwind :teleport :thief :thunder-wave :toxic :trick :trick-room :twister :u-turn :wish :zen-headbutt :baton-pass]
  :xatu [:defog :fly :giga-impact :hyper-beam :sky-attack])

(deflearnset :bw
  :natu [:aerial-ace :ally-switch :attract :calm-mind :confuse-ray :double-team :dream-eater :drill-peck :facade :faint-attack :featherdance :flash :frustration :future-sight :giga-drain :grass-knot :guard-swap :haze :heat-wave :hidden-power :leer :light-screen :lucky-chant :magic-coat :magic-room :me-first :miracle-eye :night-shade :ominous-wind :pain-split :peck :pluck :power-swap :protect :psych-up :psychic :psycho-shift :psyshock :quick-attack :rain-dance :reflect :refresh :rest :return :roost :round :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :solarbeam :steel-wing :stored-power :substitute :sucker-punch :sunny-day :swagger :synchronoise :tailwind :telekinesis :teleport :thief :thunder-wave :toxic :trick :trick-room :u-turn :wish :zen-headbutt :baton-pass]
  :xatu [:fly :foul-play :giga-impact :hyper-beam :sky-attack])
