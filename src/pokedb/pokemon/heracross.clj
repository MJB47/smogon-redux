
(in-ns 'smogon.dex)


(defpokemon :heracross
  :name "Heracross"
  :introduced-in :gs
  :types [[:bug :fighting]]
  :egggroups [[:bug]]
  :abilities [[:guts :swarm]
              :bw [:guts :moxie :swarm]]
  :stats [[80 125 75 40 95 85]]
  :weight [54]
  :height [1.5])

(deffamily (familychain :heracross))

(deflearnset :gs
  :heracross [:attract :bide :counter :curse :cut :detect :double-team :earthquake :endure :flail :frustration :fury-attack :fury-cutter :harden :headbutt :hidden-power :horn-attack :leer :megahorn :protect :rest :return :reversal :rock-smash :sleep-talk :snore :strength :sunny-day :swagger :tackle :take-down :thief :toxic])

(deflearnset :rs
  :heracross [:attract :bide :body-slam :brick-break :bulk-up :counter :cut :dig :double-edge :double-team :earthquake :endure :facade :false-swipe :flail :focus-punch :frustration :fury-attack :fury-cutter :harden :hidden-power :horn-attack :hyper-beam :leer :megahorn :mimic :protect :rain-dance :rest :return :reversal :rock-slide :rock-smash :rock-tomb :secret-power :seismic-toss :sleep-talk :snore :strength :substitute :sunny-day :swagger :swords-dance :tackle :take-down :thief :toxic :body-slam :mimic])

(deflearnset :dp
  :heracross [:aerial-ace :attract :bide :brick-break :bug-bite :bulk-up :captivate :close-combat :counter :cut :dig :double-edge :double-team :earthquake :endure :facade :false-swipe :feint :flail :fling :focus-blast :focus-punch :frustration :fury-attack :fury-cutter :giga-impact :harden :headbutt :helping-hand :hidden-power :horn-attack :hyper-beam :iron-defense :knock-off :leer :low-kick :megahorn :natural-gift :night-slash :protect :pursuit :rain-dance :rest :return :revenge :reversal :rock-slide :rock-smash :rock-tomb :secret-power :shadow-claw :sleep-talk :snore :stone-edge :strength :substitute :sunny-day :swagger :swords-dance :tackle :take-down :thief :toxic :vacuum-wave :body-slam :mimic])

(deflearnset :bw
  :heracross [:aerial-ace :attract :bide :brick-break :bug-bite :bulk-up :bulldoze :chip-away :close-combat :counter :cut :dig :double-edge :double-team :earthquake :endure :facade :false-swipe :feint :flail :fling :focus-blast :focus-punch :frustration :fury-attack :giga-impact :harden :helping-hand :hidden-power :horn-attack :hyper-beam :iron-defense :knock-off :leer :low-kick :megahorn :night-slash :protect :pursuit :rain-dance :rest :retaliate :return :revenge :reversal :rock-slide :rock-smash :rock-tomb :round :seismic-toss :shadow-claw :sleep-talk :smack-down :snore :stone-edge :strength :struggle-bug :substitute :sunny-day :swagger :swords-dance :tackle :take-down :thief :toxic :venoshock :work-up :body-slam :mimic])
