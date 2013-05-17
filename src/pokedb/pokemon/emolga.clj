
(in-ns 'smogon.dex)


(defpokemon :emolga
  :name "Emolga"
  :introduced-in :bw
  :types [[:electric :flying]]
  :egggroups [[:ground]]
  :abilities [[:motor-drive :static]]
  :stats [[55 75 60 75 60 103]]
  :weight [5]
  :height [0.4])

(deffamily (familychain :emolga))

(deflearnset :bw
  :emolga [:acrobatics :aerial-ace :agility :air-slash :astonish :attract :baton-pass :charge :charge-beam :charm :covet :cut :discharge :double-team :electro-ball :encore :facade :flash :fling :frustration :helping-hand :hidden-power :iron-tail :knock-off :last-resort :light-screen :protect :pursuit :quick-attack :rain-dance :rest :return :roost :round :shock-wave :signal-beam :sleep-talk :snore :spark :substitute :swagger :tail-whip :tailwind :taunt :thunder :thunder-wave :thunderbolt :thundershock :tickle :toxic :u-turn :volt-switch :wild-charge])
