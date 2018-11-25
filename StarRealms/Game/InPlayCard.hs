module StarRealms.Game.InPlayCard where

import StarRealms.Card
import StarRealms.Card.Ability
import StarRealms.Card.Base
import StarRealms.Card.Faction
import StarRealms.Card.Ship

import Mitchell.Prelude
import Num.Natural

data InPlayCard
  = InPlayCardBase InPlayBase
  | InPlayCardShip InPlayShip

data InPlayBase
  = InPlayBase
  { original :: Base
  , name     :: Text
  , faction  :: [Faction]
  , cost     :: Natural
  , defense  :: Natural
  , damage   :: Natural
  , outpost  :: Bool
  , primary  :: Maybe InPlayAbility
  , ally     :: Maybe InPlayAbility
  , scrap    :: Maybe InPlayAbility
  }

data InPlayShip
  = InPlayShip
  { original :: Ship
  , name     :: Text
  , faction  :: [Faction]
  , cost     :: Maybe Natural
  , primary  :: Ability
  , ally     :: Maybe InPlayAbility
  , scrap    :: Maybe InPlayAbility
  }

data InPlayAbility
  = Played Ability
  | Unplayed Ability

-- Derive a default InPlayCard from a Card.
cardToInPlayCard :: Card -> InPlayCard
cardToInPlayCard = \case
  CardBase card -> InPlayCardBase (baseToInPlayBase card)
  CardShip card -> InPlayCardShip (shipToInPlayShip card)

  where
    baseToInPlayBase :: Base -> InPlayBase
    baseToInPlayBase base =
      InPlayBase
        { original = base
        , name     = view (the @"name") base
        , faction  = [view (the @"faction") base]
        , cost     = view (the @"cost") base
        , defense  = view (the @"defense") base
        , damage   = 0
        , outpost  = view (the @"outpost") base
        , primary  = Unplayed <$> view (the @"primary") base
        , ally     = Unplayed <$> view (the @"ally") base
        , scrap    = Unplayed <$> view (the @"scrap") base
        }

    shipToInPlayShip :: Ship -> InPlayShip
    shipToInPlayShip ship =
      InPlayShip
        { original = ship
        , name     = view (the @"name") ship
        , faction  = maybe [] pure (view (the @"faction") ship)
        , cost     = view (the @"cost") ship
        , primary  = view (the @"primary") ship
        , ally     = Unplayed <$> view (the @"ally") ship
        , scrap    = Unplayed <$> view (the @"scrap") ship
        }
