
(in-ns 'smogon.dex)


(defpokemon :rufflet
  :name "Rufflet"
  :introduced-in :bw
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:hustle :keen-eye :sheer-force]]
  :stats [[70 83 50 37 50 60]]
  :weight [10.5]
  :height [0.5])

(defpokemon :braviary
  :name "Braviary"
  :introduced-in :bw
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:defiant :keen-eye :sheer-force]]
  :stats [[100 123 75 57 75 80]]
  :weight [41]
  :height [1.5])

(deffamily (familychain :rufflet :braviary))

(deflearnset :bw
  :rufflet [:aerial-ace :air-slash :attract :brave-bird :bulk-up :crush-claw :cut :defog :double-team :facade :fly :frustration :fury-attack :heat-wave :hidden-power :hone-claws :leer :peck :pluck :protect :rain-dance :rest :retaliate :return :rock-slide :rock-smash :rock-tomb :roost :round :scary-face :shadow-claw :sky-drop :slash :sleep-talk :snore :strength :substitute :sunny-day :superpower :swagger :tailwind :thrash :toxic :u-turn :whirlwind :wing-attack :work-up]
  :braviary [:giga-impact :hyper-beam :sky-attack])
