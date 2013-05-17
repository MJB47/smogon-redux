
(in-ns 'smogon.dex)

(defpokemon :diglett
  :name "Diglett"
  :introduced-in :rb
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:arena-trap :sand-veil]
              :bw [:arena-trap :sand-force :sand-veil]]
  :stats [[10 55 25 45 95]
          :gs [10 55 25 35 45 95]]
  :weight [0.8]
  :height [0.2])

(defpokemon :dugtrio
  :name "Dugtrio"
  :introduced-in :rb
  :types [[:ground]]
  :egggroups [[:ground]]
  :abilities [[:arena-trap :sand-veil]
              :bw [:arena-trap :sand-force :sand-veil]]
  :stats [[35 80 50 70 120]
          :gs [35 80 50 50 70 120]]
  :weight [33.3]
  :height [0.7])

(deffamily (familychain :diglett :dugtrio))

(deflearnset :rb
  :diglett [:bide :body-slam :cut :dig :double-edge :double-team :earthquake :fissure :growl :mimic :rage :rest :rock-slide :sand-attack :scratch :slash :substitute :take-down :toxic]
  :dugtrio [:hyper-beam])

(deflearnset :gs
  :diglett [:ancientpower :attract :beat-up :curse :cut :dig :double-team :earthquake :endure :faint-attack :fissure :frustration :growl :hidden-power :magnitude :mud-slap :protect :pursuit :rest :return :rock-smash :sand-attack :scratch :screech :slash :sleep-talk :sludge-bomb :snore :sunny-day :swagger :thief :toxic]
  :dugtrio [:hyper-beam :tri-attack])

(deflearnset :rs
  :diglett [:aerial-ace :ancientpower :attract :beat-up :body-slam :cut :dig :double-edge :double-team :earthquake :endure :facade :faint-attack :fissure :frustration :fury-cutter :fury-swipes :growl :hidden-power :magnitude :mimic :mud-slap :protect :pursuit :rest :return :rock-slide :rock-smash :rock-tomb :sand-attack :scratch :screech :secret-power :slash :sleep-talk :sludge-bomb :snore :substitute :sunny-day :swagger :thief :toxic :uproar]
  :dugtrio [:charm :hyper-beam :sand-tomb :tri-attack])

(deflearnset :dp
  :diglett [:aerial-ace :ancientpower :astonish :attract :beat-up :body-slam :captivate :cut :dig :double-edge :double-team :earth-power :earthquake :endure :facade :faint-attack :fissure :frustration :fury-cutter :growl :hidden-power :magnitude :mimic :mud-bomb :mud-slap :natural-gift :protect :pursuit :rest :return :reversal :rock-slide :rock-smash :rock-tomb :sand-attack :sandstorm :scratch :screech :secret-power :shadow-claw :slash :sleep-talk :sludge-bomb :snore :stealth-rock :substitute :sucker-punch :sunny-day :swagger :thief :toxic :uproar]
  :dugtrio [:charm :giga-impact :hyper-beam :night-slash :sand-tomb :stone-edge :tri-attack])

(deflearnset :bw
  :diglett [:aerial-ace :ancientpower :astonish :attract :beat-up :bulldoze :cut :dig :double-edge :double-team :earth-power :earthquake :echoed-voice :endure :facade :faint-attack :final-gambit :fissure :frustration :fury-cutter :growl :headbutt :hidden-power :hone-claws :magnitude :memento :mimic :mud-bomb :mud-slap :protect :pursuit :rest :return :reversal :rock-slide :rock-smash :rock-tomb :round :sand-attack :sandstorm :scratch :screech :shadow-claw :slash :sleep-talk :sludge-bomb :snore :stealth-rock :substitute :sucker-punch :sunny-day :swagger :thief :toxic :uproar]
  :dugtrio [:charm :giga-impact :hyper-beam :night-slash :sand-tomb :sludge-wave :stone-edge :tri-attack])
