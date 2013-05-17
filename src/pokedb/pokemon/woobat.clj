
(in-ns 'smogon.dex)


(defpokemon :woobat
  :name "Woobat"
  :introduced-in :bw
  :types [[:flying :psychic]]
  :egggroups [[:flying :ground]]
  :abilities [[:klutz :simple :unaware]]
  :stats [[55 45 43 55 43 72]]
  :weight [2.1]
  :height [0.4])

(defpokemon :swoobat
  :name "Swoobat"
  :introduced-in :bw
  :types [[:flying :psychic]]
  :egggroups [[:flying :ground]]
  :abilities [[:klutz :simple :unaware]]
  :stats [[67 57 55 77 55 114]]
  :weight [10.5]
  :height [0.9])

(deffamily (familychain :woobat :swoobat))



(deflearnset :bw
  :woobat [:acrobatics :aerial-ace :after-you :air-cutter :air-slash :amnesia :assurance :attract :calm-mind :charge-beam :charm :confusion :double-team :dream-eater :embargo :endeavor :energy-ball :facade :fake-tears :flash :flatter :fly :frustration :future-sight :giga-drain :gust :gyro-ball :heart-stamp :heat-wave :helping-hand :hidden-power :imprison :knock-off :light-screen :magic-coat :odor-sleuth :pluck :protect :psych-up :psychic :psyshock :rain-dance :reflect :rest :return :roost :round :safeguard :shadow-ball :signal-beam :skill-swap :sleep-talk :snore :stored-power :substitute :super-fang :supersonic :swagger :synchronoise :tailwind :taunt :telekinesis :thief :thunder-wave :torment :toxic :trick :trick-room :u-turn :uproar :zen-headbutt]
  :swoobat [:giga-impact :hyper-beam :sky-attack])

