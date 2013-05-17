
(in-ns 'smogon.dex)


(defpokemon :pinsir
  :name "Pinsir"
  :introduced-in :rb
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:hyper-cutter]
              :dp [:hyper-cutter :mold-breaker]
              :bw [:hyper-cutter :mold-breaker :moxie]]
  :stats [[65 125 100 55 85]
          :gs [65 125 100 55 70 85]]
  :weight [55]
  :height [1.5])

(deffamily (familychain :pinsir))

(deflearnset :rb
  :pinsir [:bide :bind :body-slam :cut :double-edge :double-team :focus-energy :guillotine :harden :hyper-beam :mimic :rage :rest :seismic-toss :slash :strength :submission :substitute :swords-dance :take-down :toxic :vicegrip])

(deflearnset :gs
  :pinsir [:attract :bind :curse :cut :double-team :endure :flail :focus-energy :frustration :fury-attack :fury-cutter :guillotine :harden :headbutt :hidden-power :hyper-beam :protect :rest :return :rock-smash :seismic-toss :sleep-talk :snore :strength :submission :sunny-day :swagger :swords-dance :thief :toxic :vicegrip])

(deflearnset :rs
  :pinsir [:attract :bind :body-slam :brick-break :bulk-up :cut :dig :double-edge :double-team :earthquake :endure :facade :faint-attack :false-swipe :flail :focus-energy :focus-punch :frustration :fury-attack :fury-cutter :guillotine :harden :hidden-power :hyper-beam :mimic :protect :rain-dance :rest :return :revenge :rock-slide :rock-smash :rock-tomb :secret-power :seismic-toss :sleep-talk :snore :strength :submission :substitute :sunny-day :swagger :swords-dance :thief :toxic :vicegrip :body-slam :double-edge :helping-hand :mimic])

(deflearnset :dp
  :pinsir [:attract :bind :brick-break :bulk-up :captivate :close-combat :cut :dig :double-team :earthquake :endure :facade :faint-attack :false-swipe :feint :flail :fling :focus-blast :focus-energy :focus-punch :frustration :fury-attack :fury-cutter :giga-impact :guillotine :harden :headbutt :hidden-power :hyper-beam :iron-defense :knock-off :natural-gift :protect :quick-attack :rain-dance :rest :return :revenge :rock-climb :rock-slide :rock-smash :rock-tomb :secret-power :seismic-toss :sleep-talk :snore :stealth-rock :stone-edge :strength :string-shot :submission :substitute :sunny-day :superpower :swagger :swords-dance :thief :thrash :toxic :vicegrip :vital-throw :x-scissor :body-slam :double-edge :helping-hand :mimic])

(deflearnset :bw
  :pinsir [:attract :bind :brick-break :bug-bite :bulk-up :bulldoze :close-combat :cut :dig :double-team :earthquake :facade :faint-attack :false-swipe :feint :flail :fling :focus-blast :focus-energy :frustration :fury-attack :giga-impact :guillotine :harden :hidden-power :hyper-beam :iron-defense :knock-off :me-first :protect :quick-attack :rain-dance :rest :return :revenge :rock-slide :rock-smash :rock-tomb :round :seismic-toss :sleep-talk :smack-down :snore :stealth-rock :stone-edge :storm-throw :strength :struggle-bug :submission :substitute :sunny-day :superpower :swagger :swords-dance :thief :thrash :toxic :vicegrip :vital-throw :x-scissor :body-slam :double-edge :helping-hand :mimic])
