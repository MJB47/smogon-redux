
(in-ns 'smogon.dex)


(defpokemon :larvesta
  :name "Larvesta"
  :introduced-in :bw
  :types [[:bug :fire]]
  :egggroups [[:bug]]
  :abilities [[:flame-body :swarm]]
  :stats [[55 85 55 50 55 60]]
  :weight [28.8]
  :height [1.1])

(defpokemon :volcarona
  :name "Volcarona"
  :introduced-in :bw
  :types [[:bug :fire]]
  :egggroups [[:bug]]
  :abilities [[:flame-body :swarm]]
  :stats [[85 60 65 135 105 100]]
  :weight [46]
  :height [1.6])

(deffamily (familychain :larvesta :volcarona))

(deflearnset :bw
  :larvesta [:acrobatics :amnesia :bug-bite :bug-buzz :calm-mind :double-edge :double-team :ember :endure :facade :fire-blast :flame-charge :flame-wheel :flamethrower :flare-blitz :foresight :frustration :giga-drain :harden :heat-wave :hidden-power :incinerate :leech-life :light-screen :magnet-rise :morning-sun :overheat :protect :psychic :rest :return :round :safeguard :signal-beam :sleep-talk :snore :solarbeam :string-shot :struggle-bug :substitute :sunny-day :swagger :take-down :thrash :toxic :u-turn :wild-charge :will-o-wisp :zen-headbutt]
  :volcarona [:aerial-ace :fiery-dance :fire-spin :fly :giga-impact :gust :hurricane :hyper-beam :poison-jab :quiver-dance :rage-powder :roost :silver-wind :tailwind :whirlwind])
