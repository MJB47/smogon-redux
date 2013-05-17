
(in-ns 'smogon.dex)


(defpokemon :cryogonal
  :name "Cryogonal"
  :introduced-in :bw
  :types [[:ice]]
  :egggroups [[:mineral]]
  :abilities [[:levitate]]
  :stats [[70 50 30 95 135 105]]
  :weight [148]
  :height [1.1])

(deffamily (familychain :cryogonal))

(deflearnset :bw
  :cryogonal [:acid-armor :acrobatics :attract :aurora-beam :bind :blizzard :confuse-ray :double-team :explosion :facade :flash-cannon :frost-breath :frustration :hail :haze :hidden-power :hyper-beam :ice-beam :ice-shard :icy-wind :iron-defense :knock-off :light-screen :magic-coat :mist :night-slash :poison-jab :protect :rain-dance :rapid-spin :recover :reflect :rest :return :round :sharpen :sheer-cold :signal-beam :slash :sleep-talk :snore :solarbeam :substitute :swagger :toxic])
