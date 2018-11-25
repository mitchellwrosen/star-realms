module StarRealms.Location where

import Mitchell.Prelude

data Location
  = LocationHand
  | LocationDiscardPile
  | LocationTradeRow
  deriving stock (Eq)
