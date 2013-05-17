
(in-ns 'smogon.dex)


(defpokemon :minccino
  :name "Minccino"
  :introduced-in :bw
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:cute-charm :skill-link :technician]]
  :stats [[55 50 40 40 40 75]]
  :weight [5.8]
  :height [0.4])

(defpokemon :cinccino
  :name "Cinccino"
  :introduced-in :bw
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:cute-charm :skill-link :technician]]
  :stats [[75 95 60 65 60 115]]
  :weight [7.5]
  :height [0.5])

(deffamily (familychain :minccino :cinccino))

(deflearnset :bw
  :minccino [:after-you :aqua-tail :attract :calm-mind :captivate :charm :covet :dig :double-team :doubleslap :echoed-voice :encore :endure :facade :fake-tears :flail :fling :frustration :grass-knot :growl :gunk-shot :helping-hand :hidden-power :hyper-voice :iron-tail :knock-off :last-resort :mud-slap :pound :protect :rain-dance :rest :retaliate :return :round :safeguard :seed-bomb :sing :slam :sleep-talk :snore :substitute :sunny-day :swagger :swift :tail-slap :tail-whip :thief :thunder-wave :thunderbolt :tickle :toxic :u-turn :uproar :wake-up-slap :work-up]
  :cinccino [:bullet-seed :focus-blast :giga-impact :hyper-beam :light-screen :rock-blast :thunder])
