module StarRealms.Card.Base where

import StarRealms.Card.Ability
import StarRealms.Card.Faction

import Mitchell.Prelude
import Num.Natural

data Base
  = Base
  { name    :: Text
  , faction :: Faction
  , cost    :: Natural
  , defense :: Natural
  , outpost :: Bool
  , primary :: Maybe Ability
  , ally    :: Maybe Ability
  , scrap   :: Maybe Ability
  } deriving stock (Generic)
