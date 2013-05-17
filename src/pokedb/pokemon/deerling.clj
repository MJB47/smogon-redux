
(in-ns 'smogon.dex)


(defpokemon :deerling
  :name "Deerling"
  :introduced-in :bw
  :types [[:grass :normal]]
  :egggroups [[:ground]]
  :abilities [[:chlorophyll :sap-sipper :serene-grace]]
  :stats [[60 60 50 40 50 75]]
  :weight [19.5]
  :height [0.6])

(defpokemon :sawsbuck
  :name "Sawsbuck"
  :introduced-in :bw
  :types [[:grass :normal]]
  :egggroups [[:ground]]
  :abilities [[:chlorophyll :sap-sipper :serene-grace]]
  :stats [[80 100 70 60 70 95]]
  :weight [92.5]
  :height [1.9])

(deffamily (familychain :deerling :sawsbuck))

(deflearnset :bw
  :deerling [:agility :aromatherapy :attract :baton-pass :bounce :camouflage :charm :double-edge :double-kick :double-team :echoed-voice :energy-ball :facade :faint-attack :fake-tears :flash :frustration :giga-drain :grass-knot :grasswhistle :growl :hidden-power :jump-kick :last-resort :leech-seed :light-screen :natural-gift :nature-power :odor-sleuth :protect :rain-dance :rest :retaliate :return :round :safeguard :sand-attack :seed-bomb :shadow-ball :sleep-talk :snore :solarbeam :substitute :sunny-day :swagger :synthesis :tackle :take-down :thunder-wave :toxic :wild-charge :work-up :worry-seed]
  :sawsbuck [:cut :giga-impact :horn-leech :hyper-beam :megahorn :rock-smash :swords-dance])
