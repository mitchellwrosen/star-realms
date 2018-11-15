module StarRealms.Location where

data Location
  = LocationHand
  | LocationDiscardPile
  | LocationTradeRow
  | LocationOr Location Location
