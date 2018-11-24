module StarRealms.Player where

import StarRealms.Card
import StarRealms.Game.InPlayCard

import Mitchell.Prelude
import Num.Natural

data Player
  = Player
  { authority :: Natural
  , combat    :: Natural
  , trade     :: Natural
  , hand      :: [Card]
  , deck      :: [Card]
  , discard   :: [Card]
  , inPlay    :: [InPlayCard]
  } deriving stock (Generic)
