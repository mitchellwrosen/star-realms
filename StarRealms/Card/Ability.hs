module StarRealms.Card.Ability where

import StarRealms.Location
import StarRealms.Card.Choice
import StarRealms.Condition

import Num.Natural
import Mitchell.Prelude

data Ability
  -- | Acquire any ship without paying its cost and put it on top of your deck.
  = AbilityAcquire
  | AbilityAnd Ability Ability
  | AbilityAndOr Ability Ability
  | AbilityAuthority Natural
  | AbilityCombat Natural
  | AbilityConditional Condition Ability
  | AbilityDestroyBase
  -- | Discard up to @n@ cards, then draw that many cards.
  | AbilityDiscardThenDraw Natural
  | AbilityDraw Natural
  | AbilityDrawCardPerBlobPlayed
  -- | You may...
  | AbilityMay Ability
  -- | You may put the next ship you acquire this turn on top of your deck.
  | AbilityNextShipOnTop
  -- | Target opponent discards a card.
  | AbilityOpponentDiscards
  | AbilityOr Ability Ability
  -- | Scrap a card in the ______.
  | AbilityScrap [Location]
  | AbilityStealthNeedle
  | AbilityTrade Natural
  -- | Put the next ship you acquire this turn on top of your deck.

data AbilityType
  = AbilityTypePrimary
  | AbilityTypeAlly
  | AbilityTypeScrap
