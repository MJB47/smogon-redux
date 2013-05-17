
(in-ns 'smogon.dex)


(defpokemon :kricketot
  :name "Kricketot"
  :introduced-in :dp
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:run-away :shed-skin]]
  :stats [[37 25 41 25 41 25]]
  :weight [2.2]
  :height [0.3])

(defpokemon :kricketune
  :name "Kricketune"
  :introduced-in :dp
  :types [[:bug]]
  :egggroups [[:bug]]
  :abilities [[:swarm :technician]]
  :stats [[77 85 51 55 51 65]]
  :weight [25.5]
  :height [1])

(deffamily (familychain :kricketot :kricketune))

(deflearnset :dp
  :kricketot [:bide :bug-bite :endeavor :growl :mud-slap :snore :string-shot :uproar]
  :kricketune [:aerial-ace :attract :brick-break :bug-buzz :captivate :cut :double-team :endure :facade :false-swipe :flash :focus-energy :frustration :fury-cutter :giga-impact :hidden-power :hyper-beam :knock-off :leech-life :natural-gift :night-slash :perish-song :protect :rain-dance :rest :return :rock-smash :screech :secret-power :silver-wind :sing :slash :sleep-talk :strength :substitute :sunny-day :swagger :swords-dance :taunt :toxic :x-scissor])

(deflearnset :bw
  :kricketot [:bide :bug-bite :endeavor :growl :snore :struggle-bug :uproar]
  :kricketune [:aerial-ace :attract :brick-break :bug-buzz :cut :double-team :echoed-voice :facade :false-swipe :flash :focus-energy :frustration :fury-cutter :giga-impact :heal-bell :hidden-power :hone-claws :hyper-beam :hyper-voice :knock-off :leech-life :night-slash :perish-song :protect :rain-dance :rest :return :rock-smash :round :screech :sing :slash :sleep-talk :strength :substitute :sunny-day :swagger :swords-dance :taunt :toxic :x-scissor])
