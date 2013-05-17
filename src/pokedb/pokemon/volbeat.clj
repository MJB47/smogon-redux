
(in-ns 'smogon.dex)


(defpokemon :volbeat
  :name "Volbeat"
  :introduced-in :rs
  :types [[:bug]]
  :egggroups [[:bug :humanshape]]
  :abilities [[:illuminate :swarm]
              :bw [:illuminate :prankster :swarm]]
  :stats [[65 73 55 47 75 85]]
  :weight [17.7]
  :height [0.7])

(deffamily (familychain :volbeat))

(deflearnset :rs
  :volbeat [:aerial-ace :attract :baton-pass :body-slam :brick-break :confuse-ray :counter :double-edge :double-team :dynamicpunch :endure :facade :flash :focus-punch :frustration :giga-drain :helping-hand :hidden-power :ice-punch :light-screen :mega-kick :mega-punch :metronome :mimic :moonlight :mud-slap :protect :psych-up :quick-attack :rain-dance :rest :return :secret-power :seismic-toss :shadow-ball :shock-wave :signal-beam :silver-wind :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :swift :tackle :tail-glow :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :water-pulse :body-slam :mimic])

(deflearnset :dp
  :volbeat [:aerial-ace :air-cutter :attract :baton-pass :brick-break :bug-bite :bug-buzz :captivate :charge-beam :confuse-ray :double-edge :double-team :encore :endure :facade :flash :fling :focus-punch :frustration :giga-drain :helping-hand :hidden-power :ice-punch :light-screen :moonlight :mud-slap :natural-gift :ominous-wind :protect :psych-up :quick-attack :rain-dance :rest :return :roost :secret-power :shadow-ball :shock-wave :signal-beam :silver-wind :sleep-talk :snore :solarbeam :string-shot :substitute :sunny-day :swagger :swift :tackle :tail-glow :tailwind :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :u-turn :water-pulse :zen-headbutt :body-slam :mimic])

(deflearnset :bw
  :volbeat [:acrobatics :aerial-ace :attract :baton-pass :brick-break :bug-bite :bug-buzz :charge-beam :confuse-ray :dizzy-punch :double-edge :double-team :encore :facade :flash :fling :frustration :giga-drain :helping-hand :hidden-power :ice-punch :light-screen :moonlight :protect :psych-up :quick-attack :rain-dance :rest :return :roost :round :seismic-toss :shadow-ball :signal-beam :silver-wind :sleep-talk :snore :solarbeam :struggle-bug :substitute :sunny-day :swagger :tackle :tail-glow :tailwind :thief :thunder :thunder-wave :thunderbolt :thunderpunch :toxic :trick :u-turn :zen-headbutt :body-slam :mimic])
