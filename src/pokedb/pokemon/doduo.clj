
(in-ns 'smogon.dex)

(defpokemon :doduo
  :name "Doduo"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:early-bird :run-away]
              :bw [:early-bird :run-away :tangled-feet]]
  :stats [[35 85 45 35 75]
          :gs [35 85 45 35 35 75]]
  :weight [39.2]
  :height [1.4])

(defpokemon :dodrio
  :name "Dodrio"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:early-bird :run-away]
              :bw [:early-bird :run-away :tangled-feet]]
  :stats [[60 110 70 60 100]
          :gs [60 110 70 60 60 100]]
  :weight [85.2]
  :height [1.8])

(deffamily (familychain :doduo :dodrio))

(deflearnset :rb
  :doduo [:agility :bide :body-slam :double-edge :double-team :drill-peck :fly :fury-attack :growl :mimic :peck :rage :reflect :rest :skull-bash :sky-attack :substitute :take-down :toxic :tri-attack :whirlwind :low-kick]
  :dodrio [:hyper-beam])

(deflearnset :gs
  :doduo [:agility :attract :curse :double-team :drill-peck :endure :faint-attack :flail :fly :frustration :fury-attack :growl :haze :hidden-power :mud-slap :peck :protect :pursuit :quick-attack :rage :rest :return :sleep-talk :snore :steel-wing :sunny-day :supersonic :swagger :swift :thief :toxic :tri-attack :low-kick]
  :dodrio [:hyper-beam])

(deflearnset :rs
  :doduo [:aerial-ace :agility :attract :body-slam :double-edge :double-team :drill-peck :endeavor :endure :facade :faint-attack :flail :fly :frustration :fury-attack :growl :haze :hidden-power :mimic :mud-slap :peck :protect :pursuit :quick-attack :rage :rest :return :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :swift :thief :toxic :tri-attack :uproar]
  :dodrio [:hyper-beam :taunt :torment :baton-pass])

(deflearnset :dp
  :doduo [:acupressure :aerial-ace :agility :air-cutter :attract :body-slam :brave-bird :captivate :double-hit :double-team :drill-peck :endeavor :endure :facade :faint-attack :flail :fly :frustration :fury-attack :growl :haze :hidden-power :knock-off :mimic :mirror-move :mud-slap :natural-gift :peck :pluck :protect :pursuit :quick-attack :rage :rest :return :roost :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :swift :thief :toxic :uproar]
  :dodrio [:baton-pass :giga-impact :hyper-beam :payback :sky-attack :taunt :torment :tri-attack :baton-pass])

(deflearnset :bw
  :doduo [:acupressure :aerial-ace :agility :assurance :attract :body-slam :brave-bird :double-edge :double-hit :double-team :drill-peck :echoed-voice :endeavor :endure :facade :faint-attack :flail :fly :frustration :fury-attack :growl :haze :hidden-power :knock-off :mimic :mirror-move :natural-gift :peck :pluck :protect :pursuit :quick-attack :rage :rest :return :roost :round :sky-attack :sleep-talk :snore :substitute :sunny-day :supersonic :swagger :thief :thrash :toxic :uproar :work-up]
  :dodrio [:baton-pass :giga-impact :hyper-beam :payback :sky-attack :taunt :torment :tri-attack :baton-pass])
