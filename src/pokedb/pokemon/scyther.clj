
(in-ns 'smogon.dex)


(defpokemon :scyther
  :name "Scyther"
  :introduced-in :rb
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:swarm]
              :dp [:swarm :technician]
              :bw [:steadfast :swarm :technician]]
  :stats [[70 110 80 55 105]
          :gs [70 110 80 55 80 105]]
  :weight [56]
  :height [1.5])

(defpokemon :scizor
  :name "Scizor"
  :introduced-in :gs
  :types [[:bug :steel]]
  :egggroups [[:bug]]
  :abilities [[:swarm]
              :dp [:swarm :technician]
              :bw [:light-metal :swarm :technician]]
  :stats [[70 130 100 55 80 65]]
  :weight [118]
  :height [1.8])

(deffamily (familychain :scyther :scizor))

(deflearnset :rb
  :scyther [:agility :bide :cut :double-edge :double-team :focus-energy :hyper-beam :leer :mimic :quick-attack :rage :rest :skull-bash :slash :substitute :swift :swords-dance :take-down :toxic :wing-attack])

(deflearnset :gs
  :scyther [:agility :attract :baton-pass :counter :curse :cut :detect :double-team :endure :false-swipe :focus-energy :frustration :fury-cutter :headbutt :hidden-power :hyper-beam :leer :light-screen :protect :pursuit :quick-attack :razor-wind :rest :return :reversal :rock-smash :safeguard :slash :sleep-talk :snore :steel-wing :sunny-day :swagger :swift :swords-dance :thief :toxic :wing-attack]
  :scizor [:metal-claw :sandstorm :strength])

(deflearnset :rs
  :scyther [:aerial-ace :agility :attract :baton-pass :counter :cut :double-edge :double-team :endure :facade :false-swipe :focus-energy :frustration :fury-cutter :hidden-power :hyper-beam :leer :light-screen :mimic :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :reversal :rock-smash :safeguard :secret-power :silver-wind :slash :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :swords-dance :thief :toxic :wing-attack :double-edge :mimic :morning-sun]
  :scizor [:iron-defense :metal-claw :sandstorm :strength :double-edge :mimic])

(deflearnset :dp
  :scyther [:aerial-ace :agility :air-slash :attract :baton-pass :brick-break :bug-bite :bug-buzz :captivate :counter :cut :defog :double-hit :double-team :endure :facade :false-swipe :feint :focus-energy :frustration :fury-cutter :giga-impact :headbutt :hidden-power :hyper-beam :knock-off :leer :light-screen :natural-gift :night-slash :ominous-wind :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :reversal :rock-smash :roost :safeguard :secret-power :silver-wind :slash :sleep-talk :snore :steel-wing :substitute :sunny-day :swagger :swift :swords-dance :tailwind :thief :toxic :u-turn :vacuum-wave :wing-attack :x-scissor :double-edge :mimic :morning-sun]
  :scizor [:bullet-punch :flash-cannon :fling :iron-defense :iron-head :metal-claw :sandstorm :strength :superpower :double-edge :mimic])

(deflearnset :bw
  :scyther [:aerial-ace :agility :air-slash :attract :baton-pass :brick-break :bug-bite :bug-buzz :counter :cut :defog :double-hit :double-team :endure :facade :false-swipe :feint :focus-energy :frustration :fury-cutter :giga-impact :hidden-power :hyper-beam :knock-off :leer :light-screen :night-slash :protect :pursuit :quick-attack :rain-dance :razor-wind :rest :return :reversal :rock-smash :roost :round :safeguard :silver-wind :slash :sleep-talk :snore :steel-wing :struggle-bug :substitute :sunny-day :swagger :swords-dance :tailwind :thief :toxic :u-turn :vacuum-wave :wing-attack :x-scissor :double-edge :mimic :morning-sun]
  :scizor [:acrobatics :bullet-punch :flash-cannon :fling :iron-defense :iron-head :metal-claw :sandstorm :strength :superpower :venoshock :double-edge :mimic])
