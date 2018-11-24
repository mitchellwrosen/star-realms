module StarRealms.Card.Ability where

import StarRealms.Location

import Num.Natural

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
  | AbilityNextShipOnTop
  -- | Target opponent discards a card.
  | AbilityOpponentDiscards
  | AbilityOr Ability Ability
  -- | Scrap a card in the ______.
  | AbilityScrap [Location]
  | AbilityStealthNeedle
  | AbilityTrade Natural
  -- | Put the next ship you acquire this turn on top of your deck.

data Condition
  = ConditionTwoOrMoreBasesInPlay
