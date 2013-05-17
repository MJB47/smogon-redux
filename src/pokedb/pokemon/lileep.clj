
(in-ns 'smogon.dex)


(defpokemon :lileep
  :name "Lileep"
  :introduced-in :rs
  :types [[:grass :rock]]
  :egggroups [[:water-3]]
  :abilities [[:suction-cups]
              :bw [:storm-drain :suction-cups]]
  :stats [[66 41 77 61 87 23]]
  :weight [23.8]
  :height [1])

(defpokemon :cradily
  :name "Cradily"
  :introduced-in :rs
  :types [[:grass :rock]]
  :egggroups [[:water-3]]
  :abilities [[:suction-cups]
              :bw [:storm-drain :suction-cups]]
  :stats [[86 81 97 81 107 43]]
  :weight [60.4]
  :height [1.5])

(deffamily (familychain :lileep :cradily))

(deflearnset :rs
  :lileep [:acid :amnesia :ancientpower :astonish :attract :barrier :body-slam :bullet-seed :confuse-ray :constrict :double-edge :double-team :endure :facade :frustration :giga-drain :hidden-power :ingrain :mimic :mirror-coat :mud-slap :protect :psych-up :recover :rest :return :rock-slide :sandstorm :secret-power :sleep-talk :sludge-bomb :snore :solarbeam :spit-up :stockpile :substitute :sunny-day :swagger :swallow :toxic :body-slam :double-edge :mimic]
  :cradily [:earthquake :hyper-beam :rock-smash :rock-tomb :strength])

(deflearnset :dp
  :lileep [:acid :amnesia :ancientpower :astonish :attract :barrier :bullet-seed :captivate :confuse-ray :constrict :curse :double-team :earth-power :endure :energy-ball :facade :flash :frustration :gastro-acid :giga-drain :grass-knot :hidden-power :ingrain :mirror-coat :mud-slap :natural-gift :pain-split :protect :recover :rest :return :rock-polish :rock-slide :rock-tomb :sandstorm :secret-power :seed-bomb :sleep-talk :sludge-bomb :snore :solarbeam :spit-up :stealth-rock :stockpile :string-shot :substitute :sunny-day :swagger :swallow :swords-dance :synthesis :tickle :toxic :worry-seed :wring-out :body-slam :double-edge :mimic]
  :cradily [:block :earthquake :giga-impact :headbutt :hyper-beam :rock-smash :stone-edge :strength])

(deflearnset :bw
  :lileep [:acid :amnesia :ancientpower :astonish :attract :barrier :bind :confuse-ray :constrict :curse :double-team :earth-power :endure :energy-ball :facade :flash :frustration :gastro-acid :giga-drain :grass-knot :hidden-power :ingrain :mega-drain :mirror-coat :pain-split :protect :recover :rest :return :rock-polish :rock-slide :rock-tomb :round :sandstorm :seed-bomb :sleep-talk :sludge-bomb :smack-down :snore :solarbeam :spit-up :stealth-rock :stockpile :substitute :sunny-day :swagger :swallow :swords-dance :synthesis :tickle :toxic :worry-seed :wring-out :body-slam :double-edge :mimic]
  :cradily [:block :bulldoze :earthquake :giga-impact :hyper-beam :rock-smash :sludge-wave :stone-edge :strength])
