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
  { name    :: Text
  , faction :: Faction
  , cost    :: Natural
  , defense :: Natural
  , outpost :: Bool
  , primary :: Maybe InPlayAbility
  , ally    :: Maybe InPlayAbility
  , scrap   :: Maybe InPlayAbility
  }

data InPlayShip
  = InPlayShip
  { name    :: Text
  , faction :: Maybe Faction
  , cost    :: Maybe Natural
  , primary :: Ability
  , ally    :: Maybe InPlayAbility
  , scrap   :: Maybe InPlayAbility
  }

data InPlayAbility
  = Played Ability
  | Unplayed Ability
