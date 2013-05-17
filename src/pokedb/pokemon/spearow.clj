
(in-ns 'smogon.dex)


(defpokemon :spearow
  :name "Spearow"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:keen-eye]
              :bw [:keen-eye :sniper]]
  :stats [[40 60 30 31 70]
          :gs [40 60 30 31 31 70]]
  :weight [2]
  :height [0.3])

(defpokemon :fearow
  :name "Fearow"
  :introduced-in :rb
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:keen-eye]
              :bw [:keen-eye :sniper]]
  :stats [[65 90 65 61 100]
          :gs [65 90 65 61 61 100]]
  :weight [38]
  :height [1.2])

(deffamily (familychain :spearow :fearow))



(deflearnset :rb
  :spearow [:agility :bide :double-edge :double-team :drill-peck :fly :fury-attack :growl :leer :mimic :mirror-move :peck :rage :razor-wind :rest :sky-attack :substitute :swift :take-down :toxic :whirlwind]
  :fearow [:hyper-beam])

(deflearnset :gs
  :spearow [:agility :attract :curse :detect :double-team :drill-peck :endure :faint-attack :false-swipe :fly :frustration :fury-attack :growl :hidden-power :leer :mirror-move :mud-slap :peck :protect :pursuit :quick-attack :rest :return :scary-face :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :thief :toxic :tri-attack]
  :fearow [:hyper-beam])

(deflearnset :rs
  :spearow [:aerial-ace :agility :astonish :attract :double-edge :double-team :drill-peck :endure :facade :faint-attack :false-swipe :fly :frustration :fury-attack :growl :hidden-power :leer :mimic :mirror-move :mud-slap :peck :protect :pursuit :quick-attack :rain-dance :rest :return :scary-face :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :thief :toxic :tri-attack :baton-pass :double-edge :mimic]
  :fearow [:hyper-beam :double-edge :mimic])

(deflearnset :dp
  :spearow [:aerial-ace :agility :air-cutter :assurance :astonish :attract :captivate :defog :double-team :drill-peck :endure :facade :faint-attack :false-swipe :featherdance :fly :frustration :fury-attack :growl :heat-wave :hidden-power :leer :mirror-move :mud-slap :natural-gift :ominous-wind :peck :pluck :protect :pursuit :quick-attack :rain-dance :rest :return :roost :scary-face :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :tailwind :thief :toxic :tri-attack :twister :u-turn :uproar :whirlwind :baton-pass :double-edge :mimic]
  :fearow [:giga-impact :hyper-beam :double-edge :mimic])

(deflearnset :bw
  :spearow [:aerial-ace :agility :assurance :astonish :attract :double-team :drill-peck :drill-run :echoed-voice :facade :faint-attack :false-swipe :featherdance :fly :frustration :fury-attack :growl :heat-wave :hidden-power :leer :mirror-move :peck :pluck :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :roost :round :scary-face :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :tailwind :thief :toxic :tri-attack :u-turn :uproar :whirlwind :work-up :baton-pass :double-edge :mimic]
  :fearow [:giga-impact :hyper-beam :double-edge :mimic])

