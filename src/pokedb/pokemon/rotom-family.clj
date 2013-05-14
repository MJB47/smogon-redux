
(in-ns 'smogon.dex)


(defpokemon :rotom
  :name "Rotom"
  :introduced-in :dp
  :types [[:electric :ghost]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[50 50 77 95 77 91]]
  :weight [0.3]
  :height [0.3])

(defpokemon :rotom-fan
  :name "Rotom-Fan"
  :introduced-in :dp
  :types [[:electric :ghost]
          :bw [:electric :flying]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[50 65 107 105 107 86]]
  :weight [0.3]
  :height [0.3])

(defpokemon :rotom-frost
  :name "Rotom-Frost"
  :introduced-in :dp
  :types [[:electric :ghost]
          :bw [:electric :ice]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[50 65 107 105 107 86]]
  :weight [0.3]
  :height [0.3])

(defpokemon :rotom-heat
  :name "Rotom-Heat"
  :introduced-in :dp
  :types [[:electric :ghost]
          :bw [:electric :fire]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[50 65 107 105 107 86]]
  :weight [0.3]
  :height [0.3])

(defpokemon :rotom-mow
  :name "Rotom-Mow"
  :introduced-in :dp
  :types [[:electric :ghost]
          :bw [:electric :grass]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[50 65 107 105 107 86]]
  :weight [0.3]
  :height [0.3])

(defpokemon :rotom-wash
  :name "Rotom-Wash"
  :introduced-in :dp
  :types [[:electric :ghost]
          :bw [:electric :water]]
  :egggroups [[:indeterminate]]
  :abilities [[:levitate]]
  :stats [[50 65 107 105 107 86]]
  :weight [0.3]
  :height [0.3])

(deffamilygraph (familyclique :rotom :rotom-fan :rotom-frost
                              :rotom-heat :rotom-mow :rotom-wash))

(deflearnset :dp
  :rotom [:astonish :charge :charge-beam :confuse-ray :dark-pulse :discharge :double-team :dream-eater :endure :facade :flash :frustration :hidden-power :light-screen :mud-slap :natural-gift :ominous-wind :pain-split :protect :psych-up :rain-dance :reflect :rest :return :secret-power :shadow-ball :shock-wave :signal-beam :sleep-talk :snatch :snore :spite :substitute :sucker-punch :sunny-day :swagger :swift :thief :thunder :thunder-wave :thunderbolt :thundershock :toxic :trick :uproar :will-o-wisp])

(deflearnset :bw
  :rotom [:astonish :charge :charge-beam :confuse-ray :dark-pulse :discharge :double-team :dream-eater :electro-ball :electroweb :facade :flash :frustration :hex :hidden-power :light-screen :ominous-wind :pain-split :protect :psych-up :rain-dance :reflect :rest :return :round :shadow-ball :shock-wave :signal-beam :sleep-talk :snatch :snore :spite :substitute :sunny-day :swagger :telekinesis :thief :thunder :thunder-wave :thunderbolt :thundershock :toxic :trick :uproar :volt-switch :will-o-wisp])
