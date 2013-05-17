
(in-ns 'smogon.dex)


(defpokemon :basculin
  :name "Basculin"
  :introduced-in :bw
  :types [[:water]]
  :egggroups [[:water-2]]
  :abilities [[:adaptability :mold-breaker :rock-head :reckless]]
  :stats [[70 92 65 80 55 98]]
  :weight [18]
  :height [1])

(deffamily (familychain :basculin))

(deflearnset :bw
  :basculin [:agility :aqua-jet :aqua-tail :attract :bite :bounce :brine :bubblebeam :chip-away :crunch :cut :dive :double-edge :double-team :endeavor :facade :final-gambit :flail :frustration :hail :headbutt :hidden-power :ice-beam :icy-wind :mud-shot :muddy-water :protect :rage :rain-dance :rest :return :revenge :round :scald :scary-face :sleep-talk :snore :soak :substitute :superpower :surf :swagger :swift :tackle :take-down :taunt :thrash :toxic :uproar :water-gun :waterfall :whirlpool :zen-headbutt])
