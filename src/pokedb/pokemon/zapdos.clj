
(in-ns 'smogon.dex)


(defpokemon :zapdos
  :name "Zapdos"
  :introduced-in :rb
  :types [[:electric :flying]]
  :egggroups [[]]
  :abilities [[:pressure]
              :bw [:lightningrod :pressure]]
  :stats [[90 90 85 125 100]
          :gs [90 90 85 125 90 100]]
  :weight [52.6]
  :height [1.6])

(deffamily (familychain :zapdos))

(deflearnset :rb
  :zapdos [:agility :bide :double-edge :double-team :drill-peck :flash :fly :hyper-beam :light-screen :mimic :rage :razor-wind :reflect :rest :sky-attack :substitute :swift :take-down :thunder :thunder-wave :thunderbolt :thundershock :toxic :whirlwind])

(deflearnset :gs
  :zapdos [:agility :curse :detect :double-team :drill-peck :endure :flash :fly :frustration :hidden-power :hyper-beam :light-screen :mud-slap :peck :protect :rain-dance :rest :return :roar :rock-smash :sandstorm :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :thundershock :toxic :zap-cannon])

(deflearnset :rs
  :zapdos [:aerial-ace :agility :charge :detect :double-edge :double-team :drill-peck :endure :facade :flash :fly :frustration :hidden-power :hyper-beam :light-screen :mimic :mud-slap :peck :protect :rain-dance :rest :return :roar :rock-smash :sandstorm :secret-power :shock-wave :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :thunder :thunder-wave :thunderbolt :thundershock :toxic :baton-pass :double-edge :extrasensory :metal-sound :mimic :sky-attack])

(deflearnset :dp
  :zapdos [:aerial-ace :agility :air-cutter :ancientpower :charge :charge-beam :defog :detect :discharge :double-team :drill-peck :endure :facade :flash :fly :frustration :giga-impact :heat-wave :hidden-power :hyper-beam :light-screen :mud-slap :natural-gift :ominous-wind :peck :pluck :protect :rain-dance :rest :return :roar :rock-smash :roost :sandstorm :secret-power :shock-wave :signal-beam :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :tailwind :thunder :thunder-wave :thunderbolt :thundershock :toxic :twister :u-turn :baton-pass :double-edge :extrasensory :metal-sound :mimic :sky-attack])

(deflearnset :bw
  :zapdos [:aerial-ace :agility :ancientpower :charge :charge-beam :detect :discharge :double-team :drill-peck :facade :flash :fly :frustration :giga-impact :heat-wave :hidden-power :hyper-beam :light-screen :peck :pluck :protect :rain-dance :rest :return :roar :rock-smash :roost :round :sandstorm :signal-beam :sky-attack :sky-drop :sleep-talk :snore :substitute :sunny-day :swagger :tailwind :thunder :thunder-wave :thunderbolt :thundershock :toxic :u-turn :volt-switch :wild-charge :zap-cannon :baton-pass :double-edge :extrasensory :metal-sound :mimic :sky-attack])
