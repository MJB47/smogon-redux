
(in-ns 'smogon.dex)


(defpokemon :gulpin
  :name "Gulpin"
  :introduced-in :rs
  :types [[:poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:liquid-ooze :sticky-hold]
              :bw [:gluttony :liquid-ooze :sticky-hold]]
  :stats [[70 43 53 43 53 40]]
  :weight [10.3]
  :height [0.4])

(defpokemon :swalot
  :name "Swalot"
  :introduced-in :rs
  :types [[:poison]]
  :egggroups [[:indeterminate]]
  :abilities [[:liquid-ooze :sticky-hold]
              :bw [:gluttony :liquid-ooze :sticky-hold]]
  :stats [[100 73 83 73 83 55]]
  :weight [80]
  :height [1.7])

(deffamily (familychain :gulpin :swalot))

(deflearnset :rs
  :gulpin [:acid-armor :amnesia :attract :body-slam :bullet-seed :counter :defense-curl :double-edge :double-team :dream-eater :dynamicpunch :encore :endure :explosion :facade :fire-punch :frustration :giga-drain :hidden-power :ice-beam :ice-punch :mimic :mud-slap :nightmare :pain-split :poison-gas :pound :protect :rain-dance :rest :return :rock-smash :rollout :secret-power :selfdestruct :shadow-ball :shock-wave :sleep-talk :sludge :sludge-bomb :smog :snatch :snore :solarbeam :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :thunderpunch :toxic :water-pulse :yawn :body-slam :double-edge :mimic :nightmare :selfdestruct :shock-wave :sing]
  :swalot [:hyper-beam :double-edge :mimic :nightmare :selfdestruct])

(deflearnset :dp
  :gulpin [:acid-armor :amnesia :attract :bullet-seed :captivate :curse :destiny-bond :double-team :dream-eater :encore :endure :explosion :facade :fire-punch :frustration :gastro-acid :giga-drain :gunk-shot :headbutt :hidden-power :ice-beam :ice-punch :mud-slap :natural-gift :pain-split :poison-gas :pound :protect :rain-dance :rest :return :rock-smash :rollout :secret-power :seed-bomb :shadow-ball :shock-wave :sleep-talk :sludge :sludge-bomb :smog :snatch :snore :solarbeam :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :thunderpunch :toxic :water-pulse :wring-out :yawn :body-slam :double-edge :mimic :nightmare :selfdestruct :shock-wave :sing]
  :swalot [:block :body-slam :earthquake :giga-impact :hyper-beam :double-edge :mimic :nightmare :selfdestruct])

(deflearnset :bw
  :gulpin [:acid-armor :acid-spray :amnesia :attract :curse :destiny-bond :double-team :dream-eater :encore :explosion :facade :fire-punch :frustration :gastro-acid :giga-drain :gunk-shot :hidden-power :ice-beam :ice-punch :mud-slap :pain-split :poison-gas :pound :protect :rain-dance :rest :return :rock-smash :round :seed-bomb :shadow-ball :sleep-talk :sludge :sludge-bomb :sludge-wave :smog :snatch :snore :solarbeam :spit-up :stockpile :strength :substitute :sunny-day :swagger :swallow :thunderpunch :toxic :venoshock :wring-out :yawn :body-slam :double-edge :mimic :nightmare :selfdestruct :shock-wave :sing]
  :swalot [:block :body-slam :bulldoze :earthquake :giga-impact :hyper-beam :double-edge :mimic :nightmare :selfdestruct])
