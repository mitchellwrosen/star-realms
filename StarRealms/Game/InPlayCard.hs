module StarRealms.Game.InPlayCard where

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
  , name    :: Text
  , faction :: [Faction]
  , cost    :: Natural
  , defense :: Natural
  , damage  :: Natural
  , outpost :: Bool
  , primary :: Maybe InPlayAbility
  , ally    :: Maybe InPlayAbility
  , scrap   :: Maybe InPlayAbility
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
