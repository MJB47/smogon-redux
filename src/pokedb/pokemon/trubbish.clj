
(in-ns 'smogon.dex)


(defpokemon :trubbish
  :name "Trubbish"
  :introduced-in :bw
  :types [[:poison]]
  :egggroups [[:mineral]]
  :abilities [[:aftermath :stench :sticky-hold]]
  :stats [[50 50 62 40 62 65]]
  :weight [31]
  :height [0.6])

(defpokemon :garbodor
  :name "Garbodor"
  :introduced-in :bw
  :types [[:poison]]
  :egggroups [[:mineral]]
  :abilities [[:aftermath :stench :weak-armor]]
  :stats [[80 95 82 60 82 75]]
  :weight [107.3]
  :height [1.9])

(deffamily (familychain :trubbish :garbodor))



(deflearnset :bw
  :trubbish [:acid-spray :amnesia :attract :clear-smog :curse :dark-pulse :double-team :doubleslap :drain-punch :explosion :facade :frustration :giga-drain :gunk-shot :haze :hidden-power :mud-sport :pain-split :payback :poison-gas :pound :protect :rain-dance :recycle :rest :return :rock-blast :rollout :round :sand-attack :seed-bomb :selfdestruct :sleep-talk :sludge :sludge-bomb :sludge-wave :snore :spikes :spite :stockpile :substitute :sunny-day :swagger :swallow :take-down :thief :toxic :toxic-spikes :venoshock]
  :garbodor [:body-slam :fling :focus-blast :giga-impact :hyper-beam :psychic :rock-polish :smack-down :solarbeam :thunderbolt])

