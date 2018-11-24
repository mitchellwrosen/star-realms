module StarRealms.Game.Action where

import StarRealms.Location
import StarRealms.Card.Ability

import Mitchell.Prelude

data Action
  = ActionPlayCardFromHand Text Choice
  | ActionUseAbilityOnCard Text AbilityType Choice
  | ActionCombat Target
  | ActionPurchaseTradeRow Text PurchaseLocation
  | ActionPurchaseExplorer PurchaseLocation
  | ActionDiscard Text
  | ActionScrap Text
  | ActionEndTurn

data Choice
  = ChoiceNone
  | ChoiceTrade
  | ChoiceAuthority
  | ChoiceCombat
  | ChoiceDraw
  | ChoiceInPlayShip Text
  | ChoiceTradeRowShip Text
  | ChoiceDestroyBase Text
  | ChoiceScrap [(Text, Location)]
  | ChoiceDiscard [(Text, Location)]
  | ChoiceAnd Choice Choice

data Target
  = TargetFace
  | TargetBase Text

data PurchaseLocation
  = PurchaseLocationDiscard
  | PurchaseLocationDeck
