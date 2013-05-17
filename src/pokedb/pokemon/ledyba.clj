
(in-ns 'smogon.dex)


(defpokemon :ledyba
  :name "Ledyba"
  :introduced-in :gs
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:early-bird :swarm]
              :bw [:early-bird :rattled :swarm]]
  :stats [[40 20 30 40 80 55]]
  :weight [10.8]
  :height [1])

(defpokemon :ledian
  :name "Ledian"
  :introduced-in :gs
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:early-bird :swarm]
              :bw [:early-bird :iron-fist :swarm]]
  :stats [[55 35 50 55 110 85]]
  :weight [35.6]
  :height [1.4])

(deffamily (familychain :ledyba :ledian))

(deflearnset :gs
  :ledyba [:agility :attract :baton-pass :bide :comet-punch :curse :dig :double-edge :double-team :dynamicpunch :endure :flash :frustration :giga-drain :headbutt :hidden-power :ice-punch :light-screen :protect :psybeam :reflect :rest :return :rollout :safeguard :sleep-talk :snore :solarbeam :sunny-day :supersonic :swagger :sweet-scent :swift :tackle :thief :thunderpunch :toxic]
  :ledian [:hyper-beam])

(deflearnset :rs
  :ledyba [:aerial-ace :agility :attract :baton-pass :bide :brick-break :comet-punch :dig :double-edge :double-team :dynamicpunch :endure :facade :flash :focus-punch :frustration :giga-drain :hidden-power :ice-punch :light-screen :mega-punch :mimic :protect :psybeam :reflect :rest :return :rollout :safeguard :secret-power :silver-wind :sleep-talk :snore :solarbeam :substitute :sunny-day :supersonic :swagger :swift :swords-dance :tackle :thief :thunderpunch :toxic :refresh]
  :ledian [:hyper-beam])

(deflearnset :dp
  :ledyba [:aerial-ace :agility :air-cutter :attract :baton-pass :bide :brick-break :bug-bite :bug-buzz :captivate :comet-punch :dig :double-edge :double-team :drain-punch :encore :endure :facade :flash :fling :focus-punch :frustration :giga-drain :headbutt :hidden-power :ice-punch :knock-off :light-screen :mach-punch :natural-gift :ominous-wind :protect :psybeam :reflect :rest :return :rollout :roost :safeguard :screech :secret-power :silver-wind :sleep-talk :snore :solarbeam :string-shot :substitute :sunny-day :supersonic :swagger :swift :swords-dance :tackle :tailwind :thief :thunderpunch :toxic :u-turn :uproar :refresh]
  :ledian [:focus-blast :giga-impact :hyper-beam :rock-smash :strength])

(deflearnset :bw
  :ledyba [:acrobatics :aerial-ace :agility :attract :baton-pass :bide :brick-break :bug-bite :bug-buzz :comet-punch :dig :dizzy-punch :double-edge :double-team :drain-punch :encore :facade :flash :fling :focus-punch :frustration :giga-drain :hidden-power :ice-punch :knock-off :light-screen :mach-punch :protect :psybeam :reflect :rest :return :roost :round :safeguard :screech :silver-wind :sleep-talk :snore :solarbeam :struggle-bug :substitute :sunny-day :supersonic :swagger :swift :swords-dance :tackle :tailwind :thief :thunderpunch :toxic :u-turn :uproar :refresh]
  :ledian [:focus-blast :giga-impact :hyper-beam :rock-smash :strength])
