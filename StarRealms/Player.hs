module StarRealms.Player where

import StarRealms.Card
import StarRealms.Game.InPlayCard
import StarRealms.Deck

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

data WhichPlayer
  = Player1
  | Player2
  deriving stock (Eq)

-- pluckCardFromHand :: Text -> Player -> Maybe (Card, Player)
-- pluckCardFromHand card player = do
--   (c, cs) <- pluckCard card (view (the @"hand") player)
--   Just (c, (the @"hand" .~ cs) player)

pluckCardFromHand :: Text -> Player -> Maybe (Card, Player)
pluckCardFromHand card player =
    unPluck $ ((the @"hand") (Pluck . pluckCard card)) player

newtype Pluck a = Pluck { unPluck :: Maybe (Card, a) }
  deriving (Functor)
