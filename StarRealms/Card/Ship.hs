module StarRealms.Card.Ship where

import StarRealms.Card.Ability
import StarRealms.Card.Faction

import Mitchell.Prelude
import Num.Natural

data Ship
  = Ship
  { name    :: Text
  , faction :: Maybe Faction
  , cost    :: Maybe Natural
  , primary :: Ability
  , ally    :: Maybe Ability
  , scrap   :: Maybe Ability
  }
