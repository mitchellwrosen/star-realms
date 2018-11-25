module StarRealms.Game.Action where

import StarRealms.Location
import StarRealms.Card.Ability
import StarRealms.Card.Choice

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

data Target
  = TargetFace
  | TargetBase Text

data PurchaseLocation
  = PurchaseLocationDiscard
  | PurchaseLocationDeck
