
(in-ns 'smogon.dex)


(defpokemon :vullaby
  :name "Vullaby"
  :introduced-in :bw
  :types [[:dark :flying]]
  :egggroups [[:flying]]
  :abilities [[:big-pecks :overcoat :weak-armor]]
  :stats [[70 55 75 45 65 60]]
  :weight [9]
  :height [0.5])

(defpokemon :mandibuzz
  :name "Mandibuzz"
  :introduced-in :bw
  :types [[:dark :flying]]
  :egggroups [[:flying]]
  :abilities [[:big-pecks :overcoat :weak-armor]]
  :stats [[110 65 105 55 95 80]]
  :weight [39.5]
  :height [1.2])

(deffamily (familychain :vullaby :mandibuzz))



(deflearnset :bw
  :vullaby [:aerial-ace :air-slash :attract :block :brave-bird :cut :dark-pulse :defog :double-team :embargo :facade :faint-attack :fake-tears :flatter :fly :foul-play :frustration :fury-attack :gust :heat-wave :hidden-power :incinerate :iron-defense :knock-off :leer :mean-look :mirror-move :nasty-plot :payback :pluck :protect :psych-up :punishment :rain-dance :rest :retaliate :return :rock-smash :rock-tomb :roost :round :scary-face :shadow-ball :sleep-talk :snarl :snatch :snore :steel-wing :substitute :sunny-day :swagger :tailwind :taunt :thief :torment :toxic :u-turn :whirlwind]
  :mandibuzz [:bone-rush :giga-impact :hyper-beam :sky-attack])

