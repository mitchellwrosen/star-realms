-- | The resolution of an ability.

module StarRealms.Game.Resolution
  ( Resolution(..)
  , GameNatural
  ) where

import StarRealms.Card.Choice
import StarRealms.Condition
import StarRealms.Location

import Mitchell.Prelude
import Num.Natural

data Resolution
  = ResolutionAcquire Text
  | ResolutionAnd Resolution Resolution
  | ResolutionAuthority Natural
  | ResolutionCardsIn [(Text, Location)]
  | ResolutionCombat Natural
  | ResolutionConditional Condition Resolution
  | ResolutionDestroyBase Text
  | ResolutionDiscard [Text]
  | ResolutionDraw GameNatural
  | ResolutionNextShipOnTop
  | ResolutionOpponentDiscards
  | ResolutionStealthNeedle Text Choice
  | ResolutionTrade Natural

data GameNatural
  = GameNaturalLit Natural
  | GameNaturalBlobsPlayed
