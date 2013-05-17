
(in-ns 'smogon.dex)

(defpokemon :cacnea
  :name "Cacnea"
  :introduced-in :rs
  :types [[:grass]]
  :egggroups [[:humanshape :plant]]
  :abilities [[:sand-veil]
              :bw [:sand-veil :water-absorb]]
  :stats [[50 85 40 85 40 35]]
  :weight [51.3]
  :height [0.4])

(defpokemon :cacturne
  :name "Cacturne"
  :introduced-in :rs
  :types [[:dark :grass]]
  :egggroups [[:humanshape :plant]]
  :abilities [[:sand-veil]
              :bw [:sand-veil :water-absorb]]
  :stats [[70 115 60 115 60 55]]
  :weight [77.4]
  :height [1.3])

(deffamily (familychain :cacnea :cacturne))

(deflearnset :rs
  :cacnea [:absorb :acid :attract :body-slam :bullet-seed :cotton-spore :counter :cut :destiny-bond :double-edge :double-team :dynamicpunch :endure :facade :faint-attack :flash :focus-punch :frustration :fury-cutter :giga-drain :grasswhistle :growth :hidden-power :ingrain :leech-seed :leer :mega-punch :mimic :mud-slap :needle-arm :pin-missile :poison-sting :protect :rest :return :sand-attack :sandstorm :secret-power :seismic-toss :sleep-talk :snore :solarbeam :spikes :substitute :sunny-day :swagger :swords-dance :teeter-dance :thunderpunch :toxic]
  :cacturne [:hyper-beam :mega-kick :revenge :strength])

(deflearnset :dp
  :cacnea [:absorb :acid :attract :brick-break :bullet-seed :captivate :cotton-spore :counter :cut :dark-pulse :destiny-bond :double-team :drain-punch :dynamicpunch :endure :energy-ball :facade :faint-attack :flash :fling :focus-punch :frustration :fury-cutter :giga-drain :grass-knot :grasswhistle :growth :headbutt :hidden-power :ingrain :leech-seed :leer :low-kick :magical-leaf :mud-slap :nasty-plot :natural-gift :needle-arm :payback :pin-missile :poison-jab :poison-sting :protect :rest :return :role-play :sand-attack :sandstorm :secret-power :seed-bomb :sleep-talk :smellingsalt :snore :solarbeam :spikes :spite :substitute :sucker-punch :sunny-day :swagger :swords-dance :synthesis :teeter-dance :thunderpunch :toxic :worry-seed]
  :cacturne [:embargo :focus-blast :giga-impact :hyper-beam :revenge :strength :superpower])

(deflearnset :bw
  :cacnea [:absorb :acid :attract :block :body-slam :brick-break :cotton-spore :counter :cut :dark-pulse :destiny-bond :disable :double-edge :double-team :drain-punch :dynamicpunch :energy-ball :facade :faint-attack :flash :fling :frustration :giga-drain :grass-knot :grasswhistle :growth :hidden-power :ingrain :leech-seed :leer :low-kick :magical-leaf :mimic :nasty-plot :needle-arm :payback :pin-missile :poison-jab :poison-sting :protect :rest :return :role-play :round :sand-attack :sandstorm :seed-bomb :seismic-toss :sleep-talk :smellingsalt :snore :solarbeam :spikes :spite :substitute :sucker-punch :sunny-day :swagger :switcheroo :swords-dance :synthesis :teeter-dance :thunderpunch :toxic :venoshock :worry-seed]
  :cacturne [:embargo :focus-blast :foul-play :giga-impact :hyper-beam :retaliate :revenge :strength :superpower])
