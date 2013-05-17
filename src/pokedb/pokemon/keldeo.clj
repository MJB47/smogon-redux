
(in-ns 'smogon.dex)


(defpokemon :keldeo
  :name "Keldeo"
  :introduced-in :bw
  :types [[:fighting :water]]
  :egggroups [[]]
  :abilities [[:justified]]
  :stats [[91 72 90 129 90 108]]
  :weight [48.5]
  :height [1.4])

(deffamily (familychain :keldeo))

(deflearnset :bw
  :keldeo [:aerial-ace :aqua-jet :aqua-tail :bounce :bubblebeam :calm-mind :close-combat :covet :cut :double-kick :double-team :endeavor :facade :false-swipe :focus-blast :frustration :giga-impact :hail :helping-hand :hidden-power :hydro-pump :hyper-beam :icy-wind :last-resort :leer :poison-jab :protect :psych-up :quick-guard :rain-dance :reflect :rest :retaliate :return :roar :rock-smash :round :sacred-sword :safeguard :scald :secret-sword :sleep-talk :snore :stone-edge :strength :substitute :superpower :surf :swagger :swords-dance :take-down :taunt :toxic :work-up :x-scissor])
