
(in-ns 'smogon.dex)


(defpokemon :chatot
  :name "Chatot"
  :introduced-in :dp
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:big-pecks :keen-eye :tangled-feet]]
  :stats [[76 65 45 92 42 91]]
  :weight [1.9]
  :height [0.5])

(deffamily (familychain :chatot))

(deflearnset :dp
  :chatot [:aerial-ace :agility :air-cutter :attract :captivate :chatter :defog :double-team :encore :endure :facade :featherdance :fly :frustration :fury-attack :growl :heat-wave :hidden-power :hyper-voice :mimic :mirror-move :mud-slap :nasty-plot :natural-gift :night-shade :ominous-wind :peck :pluck :protect :rain-dance :rest :return :role-play :roost :secret-power :sing :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :swift :tailwind :taunt :thief :torment :toxic :twister :u-turn :uproar])

(deflearnset :bw
  :chatot [:aerial-ace :agility :air-cutter :attract :chatter :defog :double-team :echoed-voice :encore :facade :featherdance :fly :frustration :fury-attack :growl :heat-wave :hidden-power :hyper-voice :mimic :mirror-move :nasty-plot :night-shade :peck :pluck :protect :rain-dance :rest :return :role-play :roost :round :sing :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :synchronoise :tailwind :taunt :thief :torment :toxic :u-turn :uproar :work-up])
