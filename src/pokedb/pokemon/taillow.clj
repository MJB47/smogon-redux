
(in-ns 'smogon.dex)


(defpokemon :taillow
  :name "Taillow"
  :introduced-in :rs
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:guts]
              :bw [:guts :scrappy]]
  :stats [[40 55 30 30 30 85]]
  :weight [2.3]
  :height [0.3])

(defpokemon :swellow
  :name "Swellow"
  :introduced-in :rs
  :types [[:flying :normal]]
  :egggroups [[:flying]]
  :abilities [[:guts]
              :bw [:guts :scrappy]]
  :stats [[60 85 60 50 50 125]]
  :weight [19.8]
  :height [0.7])

(deffamily (familychain :taillow :swellow))



(deflearnset :rs
  :taillow [:aerial-ace :agility :attract :counter :double-edge :double-team :endeavor :endure :facade :fly :focus-energy :frustration :growl :hidden-power :mimic :mirror-move :mud-slap :peck :protect :pursuit :quick-attack :rage :rain-dance :refresh :rest :return :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :swift :thief :toxic :wing-attack :double-edge :mimic]
  :swellow [:hyper-beam :baton-pass :double-edge :mimic])

(deflearnset :dp
  :taillow [:aerial-ace :agility :air-cutter :air-slash :attract :brave-bird :captivate :defog :double-team :endeavor :endure :facade :fly :focus-energy :frustration :growl :heat-wave :hidden-power :mirror-move :mud-slap :natural-gift :ominous-wind :peck :pluck :protect :pursuit :quick-attack :rage :rain-dance :refresh :rest :return :roost :secret-power :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :swift :tailwind :thief :toxic :twister :u-turn :whirlwind :wing-attack :double-edge :mimic]
  :swellow [:giga-impact :hyper-beam :baton-pass :double-edge :mimic])

(deflearnset :bw
  :taillow [:aerial-ace :agility :air-slash :attract :brave-bird :defog :double-team :echoed-voice :endeavor :facade :fly :focus-energy :frustration :growl :heat-wave :hidden-power :mirror-move :peck :pluck :protect :pursuit :quick-attack :rage :rain-dance :refresh :rest :return :roost :round :sky-attack :sleep-talk :snore :steel-wing :substitute :sunny-day :supersonic :swagger :tailwind :thief :toxic :u-turn :whirlwind :wing-attack :work-up :double-edge :mimic]
  :swellow [:giga-impact :hyper-beam :baton-pass :double-edge :mimic])

