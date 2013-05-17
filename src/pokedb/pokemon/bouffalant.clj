
(in-ns 'smogon.dex)


(defpokemon :bouffalant
  :name "Bouffalant"
  :introduced-in :bw
  :types [[:normal]]
  :egggroups [[:ground]]
  :abilities [[:reckless :sap-sipper :soundproof]]
  :stats [[95 110 95 40 95 55]]
  :weight [94.6]
  :height [1.6])

(deffamily (familychain :bouffalant))

(deflearnset :bw
  :bouffalant [:aerial-ace :amnesia :attract :bulldoze :cut :double-team :earthquake :endeavor :facade :focus-energy :frustration :fury-attack :giga-impact :head-charge :headbutt :hidden-power :horn-attack :iron-head :leer :megahorn :mud-shot :mud-slap :outrage :payback :poison-jab :protect :pursuit :rage :rain-dance :rest :retaliate :return :revenge :reversal :rock-climb :rock-slide :rock-smash :rock-tomb :round :scary-face :skull-bash :sleep-talk :snore :stomp :stone-edge :strength :substitute :sunny-day :superpower :surf :swagger :swords-dance :taunt :thrash :toxic :uproar :wild-charge :work-up :zen-headbutt])
