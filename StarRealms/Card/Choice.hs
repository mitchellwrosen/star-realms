module StarRealms.Card.Choice where

import StarRealms.Location

import Mitchell.Prelude

data Choice
  = ChoiceNone
  | ChoiceTrade
  | ChoiceAuthority
  | ChoiceCombat
  | ChoiceDraw
  | ChoiceCard Text
  | ChoiceCards [Text]
  | ChoiceCardsIn [(Text, Location)]
  | ChoiceAnd Choice Choice
  deriving stock (Eq)
