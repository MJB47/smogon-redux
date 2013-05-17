
(in-ns 'smogon.dex)


(defpokemon :terrakion
  :name "Terrakion"
  :introduced-in :bw
  :types [[:fighting :rock]]
  :egggroups [[]]
  :abilities [[:justified]]
  :stats [[91 129 90 72 90 108]]
  :weight [260]
  :height [1.9])

(deffamily (familychain :terrakion))

(deflearnset :bw
  :terrakion [:aerial-ace :block :bulldoze :calm-mind :close-combat :cut :double-kick :double-team :earth-power :earthquake :facade :false-swipe :focus-blast :frustration :giga-impact :helping-hand :hidden-power :hyper-beam :iron-head :leer :poison-jab :protect :psych-up :quick-attack :quick-guard :reflect :rest :retaliate :return :roar :rock-polish :rock-slide :rock-smash :rock-tomb :round :sacred-sword :safeguard :sandstorm :sleep-talk :smack-down :snore :stealth-rock :stone-edge :strength :substitute :superpower :swagger :swords-dance :take-down :taunt :toxic :work-up :x-scissor :zen-headbutt])
