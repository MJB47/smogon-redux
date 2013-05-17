
(in-ns 'smogon.dex)


(defpokemon :wurmple
  :name "Wurmple"
  :introduced-in :rs
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:shield-dust]
              :bw [:run-away :shield-dust]]
  :stats [[45 45 35 20 30 20]]
  :weight [3.6]
  :height [0.3])

(defpokemon :silcoon
  :name "Silcoon"
  :introduced-in :rs
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:shed-skin]]
  :stats [[50 35 55 25 25 15]]
  :weight [10]
  :height [0.6])

(defpokemon :beautifly
  :name "Beautifly"
  :introduced-in :rs
  :types [[:bug :flying]]
  :egggroups [[:bug]]
  :abilities [[:swarm]
              :bw [:rivalry :swarm]]
  :stats [[60 70 50 90 50 65]]
  :weight [28.4]
  :height [1])

(defpokemon :cascoon
  :name "Cascoon"
  :introduced-in :rs
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:shed-skin]]
  :stats [[50 35 55 25 25 15]]
  :weight [11.5]
  :height [0.7])

(defpokemon :dustox
  :name "Dustox"
  :introduced-in :rs
  :types [[:bug :poison]]
  :egggroups [[:bug]]
  :abilities [[:shield-dust]
              :bw [:compoundeyes :shield-dust]]
  :stats [[60 50 70 50 90 65]]
  :weight [31.6]
  :height [1.2])

(deffamily (familychain :wurmple [:silcoon :beautifly :cascoon :dustox]))



(deflearnset :rs
  :wurmple [:poison-sting :string-shot :tackle]
  :cascoon [:harden]
  :beautifly [:absorb :aerial-ace :attract :double-edge :double-team :endure :facade :flash :frustration :giga-drain :gust :hidden-power :hyper-beam :mega-drain :mimic :morning-sun :protect :psychic :rest :return :safeguard :secret-power :shadow-ball :silver-wind :sleep-talk :snore :solarbeam :stun-spore :substitute :sunny-day :swagger :swift :thief :toxic :whirlwind]
  :dustox [:confusion :light-screen :moonlight :psybeam :sludge-bomb])

(deflearnset :dp
  :wurmple [:bug-bite :poison-sting :snore :string-shot :tackle]
  :cascoon [:harden :iron-defense]
  :beautifly [:absorb :aerial-ace :air-cutter :attract :bug-buzz :captivate :defog :double-team :endure :energy-ball :facade :flash :frustration :giga-drain :giga-impact :gust :hidden-power :hyper-beam :mega-drain :morning-sun :natural-gift :ominous-wind :protect :psychic :rest :return :roost :safeguard :secret-power :shadow-ball :signal-beam :silver-wind :sleep-talk :solarbeam :stun-spore :substitute :sunny-day :swagger :swift :tailwind :thief :toxic :twister :u-turn :whirlwind]
  :dustox [:confusion :light-screen :moonlight :psybeam :sludge-bomb])

(deflearnset :bw
  :wurmple [:bug-bite :electroweb :poison-sting :snore :string-shot :tackle]
  :cascoon [:harden :iron-defense]
  :beautifly [:absorb :acrobatics :aerial-ace :attract :bug-buzz :double-team :energy-ball :facade :flash :frustration :giga-drain :giga-impact :gust :hidden-power :hyper-beam :mega-drain :morning-sun :protect :psychic :quiver-dance :rest :return :roost :round :safeguard :shadow-ball :signal-beam :silver-wind :sleep-talk :solarbeam :struggle-bug :stun-spore :substitute :sunny-day :swagger :tailwind :thief :toxic :u-turn :venoshock :whirlwind]
  :dustox [:confusion :light-screen :moonlight :psybeam :sludge-bomb])

