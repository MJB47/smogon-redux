
(in-ns 'smogon.dex)


(defpokemon :moltres
  :name "Moltres"
  :introduced-in :rb
  :types [[:fire :flying]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:flame-body :pressure]]
  :stats [[90 100 90 125 90]
          :gs [90 100 90 125 85 90]]
  :weight [60]
  :height [2])

(deffamily (familychain :moltres))

(deflearnset :rb
  :moltres [:agility :bide :double-edge :double-team :fire-blast :fire-spin :fly :hyper-beam :leer :mimic :peck :rage :razor-wind :reflect :rest :sky-attack :substitute :swift :take-down :toxic :whirlwind])

(deflearnset :gs
  :moltres [:agility :curse :detect :double-team :ember :endure :fire-blast :fire-spin :flamethrower :fly :frustration :hidden-power :hyper-beam :mud-slap :protect :rain-dance :rest :return :roar :rock-smash :safeguard :sandstorm :sky-attack :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :toxic :wing-attack])

(deflearnset :rs
  :moltres [:aerial-ace :agility :double-edge :double-team :ember :endure :facade :fire-blast :fire-spin :flamethrower :fly :frustration :heat-wave :hidden-power :hyper-beam :mimic :mud-slap :overheat :protect :rain-dance :rest :return :roar :rock-smash :safeguard :sandstorm :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :toxic :wing-attack :double-edge :extrasensory :mimic :morning-sun])

(deflearnset :dp
  :moltres [:aerial-ace :agility :air-cutter :air-slash :ancientpower :defog :double-team :ember :endure :facade :fire-blast :fire-spin :flamethrower :fly :frustration :giga-impact :heat-wave :hidden-power :hyper-beam :mud-slap :natural-gift :ominous-wind :overheat :pluck :protect :rain-dance :rest :return :roar :rock-smash :roost :safeguard :sandstorm :secret-power :sky-attack :sleep-talk :snore :solarbeam :steel-wing :substitute :sunny-day :swagger :swift :tailwind :toxic :twister :u-turn :will-o-wisp :wing-attack :double-edge :extrasensory :mimic :morning-sun])

(deflearnset :bw
  :moltres [:aerial-ace :agility :air-slash :ancientpower :double-team :ember :endure :facade :fire-blast :fire-spin :flame-charge :flamethrower :fly :frustration :giga-impact :heat-wave :hidden-power :hurricane :hyper-beam :incinerate :overheat :pluck :protect :rain-dance :rest :return :roar :rock-smash :roost :round :safeguard :sandstorm :sky-attack :sky-drop :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :tailwind :toxic :u-turn :will-o-wisp :wing-attack :double-edge :extrasensory :mimic :morning-sun])
