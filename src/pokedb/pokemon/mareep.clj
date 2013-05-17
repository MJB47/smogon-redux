
(in-ns 'smogon.dex)


(defpokemon :mareep
  :name "Mareep"
  :introduced-in :gs
  :types [[:electric]]
  :egggroups [[:ground :monster]]
  :abilities [[:static]
              :bw [:plus :static]]
  :stats [[55 40 40 65 45 35]]
  :weight [7.8]
  :height [0.6])

(defpokemon :flaaffy
  :name "Flaaffy"
  :introduced-in :gs
  :types [[:electric]]
  :egggroups [[:ground :monster]]
  :abilities [[:static]
              :bw [:plus :static]]
  :stats [[70 55 55 80 60 45]]
  :weight [13.3]
  :height [0.8])

(defpokemon :ampharos
  :name "Ampharos"
  :introduced-in :gs
  :types [[:electric]]
  :egggroups [[:ground :monster]]
  :abilities [[:static]
              :bw [:plus :static]]
  :stats [[90 75 75 115 90 55]]
  :weight [61.5]
  :height [1.4])

(deffamily (familychain :mareep :flaaffy :ampharos))

(deflearnset :gs
  :mareep [:attract :body-slam :cotton-spore :curse :defense-curl :double-team :endure :flash :frustration :growl :headbutt :hidden-power :iron-tail :light-screen :protect :rain-dance :reflect :rest :return :safeguard :screech :sleep-talk :snore :swagger :swift :tackle :take-down :thunder :thunder-wave :thunderbolt :thundershock :toxic :zap-cannon]
  :flaaffy [:dynamicpunch :fire-punch :rock-smash :strength :thunderpunch]
  :ampharos [:hyper-beam])

(deflearnset :rs
  :mareep [:attract :body-slam :charge :cotton-spore :defense-curl :double-edge :double-team :endure :facade :flash :frustration :growl :hidden-power :iron-tail :light-screen :mimic :odor-sleuth :protect :rain-dance :reflect :rest :return :safeguard :screech :secret-power :shock-wave :sleep-talk :snore :substitute :swagger :swift :tackle :take-down :thunder :thunder-wave :thunderbolt :thundershock :toxic :heal-bell]
  :flaaffy [:brick-break :counter :dynamicpunch :fire-punch :focus-punch :mega-kick :mega-punch :rock-smash :seismic-toss :strength :thunderpunch]
  :ampharos [:hyper-beam])

(deflearnset :dp
  :mareep [:attract :body-slam :captivate :charge :charge-beam :cotton-spore :discharge :double-team :endure :facade :flash :flatter :frustration :growl :headbutt :heal-bell :hidden-power :iron-tail :light-screen :magnet-rise :natural-gift :odor-sleuth :power-gem :protect :rain-dance :reflect :rest :return :safeguard :sand-attack :screech :secret-power :shock-wave :signal-beam :sleep-talk :snore :substitute :swagger :swift :tackle :take-down :thunder :thunder-wave :thunderbolt :thundershock :toxic :heal-bell]
  :flaaffy [:brick-break :fire-punch :fling :focus-punch :rock-smash :strength :thunderpunch]
  :ampharos [:focus-blast :giga-impact :hyper-beam :outrage :rock-climb])

(deflearnset :bw
  :mareep [:after-you :agility :attract :body-slam :charge :charge-beam :confuse-ray :cotton-guard :cotton-spore :discharge :double-team :echoed-voice :electro-ball :facade :flash :flatter :frustration :growl :heal-bell :hidden-power :iron-tail :light-screen :magnet-rise :odor-sleuth :power-gem :protect :rain-dance :rest :return :round :safeguard :sand-attack :screech :signal-beam :sleep-talk :snore :substitute :swagger :tackle :take-down :thunder :thunder-wave :thunderbolt :thundershock :toxic :wild-charge :heal-bell]
  :flaaffy [:brick-break :fire-punch :fling :rock-smash :strength :thunderpunch :volt-switch]
  :ampharos [:bulldoze :focus-blast :giga-impact :hyper-beam :outrage])
