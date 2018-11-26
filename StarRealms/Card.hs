module StarRealms.Card where

import StarRealms.Card.Ability
import StarRealms.Card.Base
import StarRealms.Card.Faction
import StarRealms.Card.Ship
import StarRealms.Location

import Mitchell.Prelude
import qualified Map

data Card
  = CardBase Base
  | CardShip Ship

-- | Get the name of a card.
cardName :: Lens' Card Text
cardName f = \case
  CardBase card -> CardBase <$> (the @"name") f card
  CardShip card -> CardShip <$> (the @"name") f card

cardPrimaryAbility :: Card -> Maybe Ability
cardPrimaryAbility = \case
  CardBase card -> view (the @"primary") card
  CardShip card -> Just (view (the @"primary") card)

-- | The initial player deck: 8 Scouts and 2 Vipers.
initialPlayerDeck :: [Card]
initialPlayerDeck =
  replicate 2 viper ++ replicate 8 scout

initialTradeDeck :: [Card]
initialTradeDeck =
  [ barterWorld
  , barterWorld
  , battleBlob
  , battleMech
  , battlePod
  , battlePod
  , battleStation
  , battleStation
  , battlecruiser
  , blobCarrier
  , blobDestroyer
  , blobDestroyer
  , blobFighter
  , blobFighter
  , blobFighter
  , blobWheel
  , blobWheel
  , blobWheel
  , blobWorld
  , brainWorld
  , centralOffice
  , commandShip
  , corvette
  , corvette
  , cutter
  , cutter
  , cutter
  , defenseCenter
  , dreadnaught
  , embassyYacht
  , embassyYacht
  , federationShuttle
  , federationShuttle
  , federationShuttle
  , flagship
  , fleetHQ
  , freighter
  , freighter
  , imperialFighter
  , imperialFighter
  , imperialFighter
  , imperialFrigate
  , imperialFrigate
  , imperialFrigate
  , junkyard
  , machineBase
  , mechWorld
  , missileBot
  , missileBot
  , missileBot
  , missileMech
  , mothership
  , patrolMech
  , patrolMech
  , portOfCall
  , ram
  , ram
  , recyclingStation
  , recyclingStation
  , royalRedoubt
  , spaceStation
  , spaceStation
  , stealthNeedle
  , supplyBot
  , supplyBot
  , supplyBot
  , surveyShip
  , surveyShip
  , surveyShip
  , theHive
  , tradeBot
  , tradeBot
  , tradeBot
  , tradeEscort
  , tradePod
  , tradePod
  , tradePod
  , tradingPost
  , tradingPost
  , warWorld
  ]

cardMap :: Map Text Card
cardMap = undefined

getCardByName :: Text -> Card
getCardByName card =
  case Map.lookup card cardMap of
    Nothing -> error "getCardByName: card does not exist"
    Just c -> c

battleMech :: Card
battleMech = undefined

battlecruiser :: Card
battlecruiser = undefined

blobFighter :: Card
blobFighter = undefined

blobWorld :: Card
blobWorld = undefined

brainWorld :: Card
brainWorld = undefined

commandShip :: Card
commandShip = undefined

defenseCenter :: Card
defenseCenter = undefined

dreadnaught :: Card
dreadnaught = undefined

embassyYacht :: Card
embassyYacht = undefined

flagship :: Card
flagship = undefined

fleetHQ :: Card
fleetHQ = undefined

imperialFighter :: Card
imperialFighter = undefined

junkyard :: Card
junkyard = undefined

machineBase :: Card
machineBase = undefined

mechWorld :: Card
mechWorld = undefined

missileBot :: Card
missileBot = undefined

missileMech :: Card
missileMech = undefined

patrolMech :: Card
patrolMech = undefined

spaceStation :: Card
spaceStation = undefined

stealthNeedle :: Card
stealthNeedle = undefined

tradeBot :: Card
tradeBot = undefined

tradeEscort :: Card
tradeEscort = undefined

tradingPost :: Card
tradingPost = undefined

barterWorld :: Card
barterWorld =
  CardBase Base
    { name    = "Barter World"
    , faction = TradeFederation
    , cost    = 4
    , defense = 4
    , outpost = False
    , primary = Just (AbilityOr (AbilityAuthority 2) (AbilityTrade 2))
    , ally    = Nothing
    , scrap   = Just (AbilityCombat 5)
    }

battleBlob :: Card
battleBlob =
  CardShip Ship
    { name    = "Battle Blob"
    , faction = Just Blob
    , cost    = Just 6
    , primary = AbilityCombat 8
    , ally    = Just (AbilityDraw (GameNaturalLit 1))
    , scrap   = Just (AbilityCombat 4)
    }

battlePod :: Card
battlePod =
  CardShip Ship
    { name    = "Battle Pod"
    , faction = Just Blob
    , cost    = Just 2
    , primary = AbilityAnd
                  (AbilityCombat 4)
                  (AbilityMay (AbilityScrap [LocationTradeRow]))
    , ally    = Just (AbilityCombat 2)
    , scrap   = Nothing
    }

battleStation :: Card
battleStation =
  CardBase Base
    { name    = "Battle Station"
    , faction = MachineCult
    , cost    = 3
    , defense = 5
    , outpost = True
    , primary = Nothing
    , ally    = Nothing
    , scrap   = Just (AbilityCombat 5)
    }

blobCarrier :: Card
blobCarrier =
  CardShip Ship
    { name    = "Blob Carrier"
    , faction = Just Blob
    , cost    = Just 6
    , primary = AbilityCombat 7
    , ally    = Just AbilityAcquire
    , scrap   = Nothing
    }

blobDestroyer :: Card
blobDestroyer =
  CardShip Ship
    { name    = "Blob Destroyer"
    , faction = Just Blob
    , cost    = Just 4
    , primary = AbilityCombat 6
    , ally    = Just
                  (AbilityMay
                    (AbilityAndOr
                      AbilityDestroyBase
                      (AbilityScrap [LocationTradeRow])))
    , scrap   = Nothing
    }

blobWheel :: Card
blobWheel =
  CardBase Base
    { name    = "Blob Wheel"
    , faction = Blob
    , cost    = 3
    , defense = 5
    , outpost = False
    , primary = Just (AbilityCombat 1)
    , ally    = Nothing
    , scrap   = Just (AbilityTrade 3)
    }

centralOffice :: Card
centralOffice =
  CardBase Base
    { name    = "Central Office"
    , faction = TradeFederation
    , cost    = 7
    , defense = 6
    , outpost = False
    , primary = Just
                  (AbilityAnd
                    (AbilityTrade 2)
                    AbilityNextShipOnTop)
    , ally    = Just (AbilityDraw (GameNaturalLit 1))
    , scrap   = Nothing
    }

corvette :: Card
corvette =
  CardShip Ship
    { name    = "Corvette"
    , faction = Just StarEmpire
    , cost    = Just 2
    , primary = AbilityAnd (AbilityCombat 1) (AbilityDraw (GameNaturalLit 1))
    , ally    = Just (AbilityCombat 2)
    , scrap   = Nothing
    }

cutter :: Card
cutter =
  CardShip Ship
    { name    = "Cutter"
    , faction = Just TradeFederation
    , cost    = Just 2
    , primary = AbilityAnd (AbilityAuthority 4) (AbilityTrade 2)
    , ally    = Just (AbilityCombat 4)
    , scrap   = Nothing
    }

explorer :: Card
explorer =
  CardShip Ship
    { name    = "Explorer"
    , faction = Nothing
    , cost    = Just 2
    , primary = AbilityTrade 2
    , ally    = Nothing
    , scrap   = Just (AbilityCombat 2)
    }

federationShuttle :: Card
federationShuttle =
  CardShip Ship
    { name    = "Federation Shuttle"
    , faction = Just TradeFederation
    , cost    = Just 1
    , primary = AbilityTrade 2
    , ally    = Just (AbilityAuthority 4)
    , scrap   = Nothing
    }

freighter :: Card
freighter =
  CardShip Ship
    { name    = "Freighter"
    , faction = Just TradeFederation
    , cost    = Just 4
    , primary = AbilityTrade 4
    , ally    = Just AbilityNextShipOnTop
    , scrap   = Nothing
    }

imperialFrigate :: Card
imperialFrigate =
  CardShip Ship
    { name    = "Imperial Frigate"
    , faction = Just TradeFederation
    , cost    = Just 3
    , primary = AbilityAnd (AbilityCombat 4) AbilityOpponentDiscards
    , ally    = Just (AbilityCombat 2)
    , scrap   = Just (AbilityDraw (GameNaturalLit 1))
    }

mothership :: Card
mothership =
  CardShip Ship
    { name    = "Mothership"
    , faction = Just Blob
    , cost    = Just 7
    , primary = AbilityAnd (AbilityCombat 6) (AbilityDraw (GameNaturalLit 1))
    , ally    = Just (AbilityDraw (GameNaturalLit 1))
    , scrap   = Nothing
    }

portOfCall :: Card
portOfCall =
  CardBase Base
    { name    = "Port of Call"
    , faction = TradeFederation
    , cost    = 6
    , defense = 6
    , outpost = True
    , primary = Just (AbilityTrade 3)
    , ally    = Nothing
    , scrap   = Just
                  (AbilityAnd
                    (AbilityDraw (GameNaturalLit 1))
                    (AbilityMay AbilityDestroyBase))
    }

ram :: Card
ram =
  CardShip Ship
    { name    = "Ram"
    , faction = Just Blob
    , cost    = Just 3
    , primary = AbilityCombat 5
    , ally    = Just (AbilityCombat 2)
    , scrap   = Just (AbilityTrade 3)
    }

recyclingStation :: Card
recyclingStation =
  CardBase Base
    { name    = "Recycling Station"
    , faction = StarEmpire
    , cost    = 4
    , defense = 4
    , outpost = True
    , primary = Just (AbilityOr (AbilityTrade 1) (AbilityDiscardThenDraw 2))
    , ally    = Nothing
    , scrap   = Nothing
    }

royalRedoubt :: Card
royalRedoubt =
  CardBase Base
    { name    = "Royal Redoubt"
    , faction = StarEmpire
    , cost    = 6
    , defense = 6
    , outpost = True
    , primary = Just (AbilityCombat 3)
    , ally    = Just AbilityOpponentDiscards
    , scrap   = Nothing
    }

scout :: Card
scout =
  CardShip Ship
    { name    = "Scout"
    , faction = Nothing
    , cost    = Nothing
    , primary = AbilityTrade 1
    , ally    = Nothing
    , scrap   = Nothing
    }

supplyBot :: Card
supplyBot =
  CardShip Ship
    { name    = "Supply Bot"
    , faction = Just MachineCult
    , cost    = Just 3
    , primary = AbilityAnd
                  (AbilityTrade 2)
                  (AbilityMay
                    (AbilityScrap
                      ([LocationHand, LocationDiscardPile])))
    , ally    = Just (AbilityCombat 2)
    , scrap   = Nothing
    }

surveyShip :: Card
surveyShip =
  CardShip Ship
    { name    = "Survey Ship"
    , faction = Just StarEmpire
    , cost    = Just 3
    , primary = AbilityAnd (AbilityTrade 1) (AbilityDraw (GameNaturalLit 1))
    , ally    = Nothing
    , scrap   = Just AbilityOpponentDiscards
    }

theHive :: Card
theHive =
  CardBase Base
    { name    = "The Hive"
    , faction = Blob
    , cost    = 5
    , defense = 5
    , outpost = False
    , primary = Just (AbilityCombat 3)
    , ally    = Just (AbilityDraw (GameNaturalLit 1))
    , scrap   = Nothing
    }

tradePod :: Card
tradePod =
  CardShip Ship
    { name    = "Trade Pod"
    , faction = Just Blob
    , cost    = Just 2
    , primary = AbilityTrade 3
    , ally    = Just (AbilityCombat 2)
    , scrap   = Nothing
    }


viper :: Card
viper =
  CardShip Ship
    { name    = "Viper"
    , faction = Nothing
    , cost    = Nothing
    , primary = AbilityCombat 1
    , ally    = Nothing
    , scrap   = Nothing
    }

warWorld :: Card
warWorld =
  CardBase Base
    { name    = "War World"
    , faction = StarEmpire
    , cost    = 5
    , defense = 4
    , outpost = True
    , primary = Just (AbilityCombat 3)
    , ally    = Just (AbilityCombat 4)
    , scrap   = Nothing
    }
