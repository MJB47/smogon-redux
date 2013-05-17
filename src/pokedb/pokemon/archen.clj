
(in-ns 'smogon.dex)


(defpokemon :archen
  :name "Archen"
  :introduced-in :bw
  :types [[:flying :rock]]
  :egggroups [[:flying :water-3]]
  :abilities [[:defeatist]]
  :stats [[55 112 45 74 45 70]]
  :weight [9.5]
  :height [0.5])

(defpokemon :archeops
  :name "Archeops"
  :introduced-in :bw
  :types [[:flying :rock]]
  :egggroups [[:flying :water-3]]
  :abilities [[:defeatist]]
  :stats [[75 140 65 112 65 110]]
  :weight [32]
  :height [1.4])

(deffamily
  (familychain :archen :archeops))

(deflearnset :bw
  :archen [:acrobatics :aerial-ace :agility :ancientpower :aqua-tail :attract :bite :bounce :bulldoze :crunch :cut :defog :dig :double-team :dragon-claw :dragon-pulse :dragonbreath :earth-power :earthquake :endeavor :facade :frustration :head-smash :heat-wave :hidden-power :hone-claws :iron-defense :iron-tail :knock-off :leer :pluck :protect :quick-attack :quick-guard :rest :return :roar :rock-polish :rock-slide :rock-smash :rock-throw :rock-tomb :roost :round :sandstorm :scary-face :shadow-claw :sleep-talk :smack-down :snore :stealth-rock :steel-wing :stone-edge :substitute :swagger :tailwind :taunt :thrash :torment :toxic :u-turn :uproar :wing-attack]
  :archeops [:dragon-tail :fly :focus-blast :giga-impact :hyper-beam :outrage :sky-attack])
