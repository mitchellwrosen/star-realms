module StarRealms.Player where

import StarRealms.Card
import StarRealms.Deck
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
  -- | In-play cards. Invariant: cards sorted by reverse-played-in order (the
  -- latest card played is at the head of the list).
  , inPlay    :: [InPlayCard]
  } deriving stock (Generic)

data WhichPlayer
  = Player1
  | Player2
  deriving stock (Eq)

pluckCardFromHand :: Text -> Player -> Maybe (Card, Player)
pluckCardFromHand card player =
    unPluck $ ((the @"hand") (Pluck . pluckCard card)) player

newtype Pluck a = Pluck { unPluck :: Maybe (Card, a) }
  deriving stock (Functor)
