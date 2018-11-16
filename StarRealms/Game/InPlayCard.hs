module StarRealms.Game.InPlayCard where

import StarRealms.Card.Ability
import StarRealms.Card.Faction

import Mitchell.Prelude
import Num.Natural

data InPlayCard
  = InPlayCardBase InPlayBase
  | InPlayCardShip InPlayShip

data InPlayBase
  = InPlayBase
  { name       :: Text
  , faction    :: Faction
  , cost       :: Natural
  , defense    :: Natural
  , outpost    :: Bool
  , unplayed   :: [Ability]
  , unplayable :: Maybe Ability
  , scrap      :: Maybe Ability
  }

data InPlayShip
  = InPlayShip
  { name       :: Text
  , faction    :: Maybe Faction
  , cost       :: Maybe Natural
  , ability    :: InPlayShipAbility
  , scrap      :: Maybe Ability
  }

data InPlayShipAbility
  = NoAbility
  | UnplayedAbility Ability
  | UnplayableAbility Ability
