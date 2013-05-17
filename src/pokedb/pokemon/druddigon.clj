
(in-ns 'smogon.dex)


(defpokemon :druddigon
  :name "Druddigon"
  :introduced-in :bw
  :types [[:dragon]]
  :egggroups [[:dragon :monster]]
  :abilities [[:mold-breaker :rough-skin :sheer-force]]
  :stats [[77 120 90 60 90 48]]
  :weight [139]
  :height [1.6])

(deffamily (familychain :druddigon))

(deflearnset :bw
  :druddigon [:aerial-ace :aqua-tail :attract :bite :bulldoze :charge-beam :chip-away :crunch :crush-claw :cut :dark-pulse :dig :double-team :draco-meteor :dragon-claw :dragon-pulse :dragon-rage :dragon-tail :earthquake :facade :faint-attack :fire-fang :fire-punch :flamethrower :flash-cannon :fling :focus-blast :frustration :giga-impact :glare :gunk-shot :heat-wave :hidden-power :hone-claws :hyper-beam :incinerate :iron-head :iron-tail :leer :metal-claw :night-slash :outrage :payback :poison-tail :protect :pursuit :rain-dance :rest :retaliate :return :revenge :roar :rock-climb :rock-slide :rock-smash :rock-tomb :round :scary-face :scratch :shadow-claw :slash :sleep-talk :sludge-bomb :smack-down :snarl :snatch :snore :stealth-rock :strength :substitute :sucker-punch :sunny-day :superpower :surf :swagger :taunt :thunder-fang :thunderpunch :torment :toxic])
