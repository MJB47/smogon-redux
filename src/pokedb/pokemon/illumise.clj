
(in-ns 'smogon.dex)


(defpokemon :illumise
  :name "Illumise"
  :introduced-in :rs
  :types [[:bug]]
  :egggroups [[:bug :humanshape]]
  :abilities [[:oblivious]
              :dp [:oblivious :tinted-lens]
              :bw [:oblivious :prankster :tinted-lens]]
  :stats [[65 47 55 73 75 85]]
  :weight [17.7]
  :height [0.6])

(deffamily (familychain :illumise))

(deflearnset :rs
  :illumise [:aerial-ace :attract :baton-pass :body-slam :brick-break :charm :counter :covet :double-edge :double-team :dynamicpunch :encore :endure :facade :flash :flatter :focus-punch :frustration :giga-drain :growth :helping-hand :hidden-power :ice-punch :light-screen :mega-kick :mega-punch :metronome :mimic :moonlight :mud-slap :protect :psych-up :quick-attack :rain-dance :rest :return :secret-power :seismic-toss :shadow-ball :shock-wave :silver-wind :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :sweet-scent :swift :tackle :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :water-pulse :wish :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :dp
  :illumise [:aerial-ace :air-cutter :attract :baton-pass :brick-break :bug-bite :bug-buzz :captivate :charge-beam :charm :covet :double-team :encore :endure :facade :flash :flatter :fling :focus-punch :frustration :giga-drain :growth :helping-hand :hidden-power :ice-punch :light-screen :moonlight :mud-slap :natural-gift :ominous-wind :protect :psych-up :quick-attack :rain-dance :rest :return :roost :secret-power :shadow-ball :shock-wave :silver-wind :sleep-talk :snore :solarbeam :string-shot :substitute :sunny-day :swagger :sweet-scent :swift :tackle :tailwind :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :u-turn :water-pulse :wish :zen-headbutt :body-slam :double-edge :mimic :seismic-toss])

(deflearnset :bw
  :illumise [:acrobatics :aerial-ace :attract :baton-pass :brick-break :bug-bite :bug-buzz :captivate :charge-beam :charm :confuse-ray :covet :double-team :encore :facade :fake-tears :flash :flatter :fling :frustration :giga-drain :growth :helping-hand :hidden-power :ice-punch :light-screen :moonlight :protect :psych-up :quick-attack :rain-dance :rest :return :roost :round :shadow-ball :silver-wind :sleep-talk :snore :solarbeam :struggle-bug :substitute :sunny-day :swagger :sweet-scent :tackle :tailwind :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :u-turn :wish :zen-headbutt :body-slam :double-edge :mimic :seismic-toss])
