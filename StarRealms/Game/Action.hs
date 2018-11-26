module StarRealms.Game.Action where

import StarRealms.Location
import StarRealms.Card.Ability
import StarRealms.Card.Choice

import Mitchell.Prelude

-- | An action that can be made on a game.
data Action
  -- | Assign combat to a target.
  = ActionAssignCombat Target
  -- | Discard a card.
  | ActionDiscard Text
  -- | End turn.
  | ActionEndTurn
  -- | Play a card from one's hand, and include the choice of how to resolve its
  -- primary ability.
  | ActionPlayCard Text Choice
  -- | Purchase a card from the trade row.
  | ActionPurchase Text PurchaseLocation
  -- | Purchase an explorer.
  | ActionPurchaseExplorer PurchaseLocation
  -- | Scrap a card.
  | ActionScrap Text
  -- | Use an ability on a card.
  | ActionUseAbilityOnCard Text AbilityType Choice

-- | What to target with combat points.
data Target
  = TargetFace
  | TargetBase Text

-- | Where to purchase a card to.
data PurchaseLocation
  = PurchaseLocationDiscard
  | PurchaseLocationDeck
