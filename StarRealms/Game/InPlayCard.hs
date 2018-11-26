module StarRealms.Game.InPlayCard where

import StarRealms.Card
import StarRealms.Card.Base
import StarRealms.Card.Ship

import Mitchell.Prelude
import Num.Natural

-- | An in-play card has the original card, plus some additional data that is
-- only relevant while the card is on the battlefield:
--
-- * Has the primary/ally/scrap ability been played?
-- * Has it taken damage?
data InPlayCard
  = InPlayCardBase InPlayBase
  -- | An in-play ship. Invariant: it's not Stealth Needle.
  | InPlayCardShip InPlayShip
  -- | An in-play Stealth Needle. Contains the card it has cloned, rather than
  -- Stealth Needle itself.
  | InPlayCardStealthNeedle InPlayShip

data InPlayBase
  = InPlayBase
  { original :: Base
  -- | How much damage the base has taken. Invariant: it's less than the base's
  -- defense.
  , damage   :: Natural
  , primary  :: InPlayAbility
  , ally     :: InPlayAbility
  , scrap    :: InPlayAbility
  }

data InPlayShip
  = InPlayShip
  { original :: Ship
  , ally     :: InPlayAbility
  , scrap    :: InPlayAbility
  }

data InPlayAbility
  = Played
  | Unplayed

-- Make an 'InPlayCard' from a 'Card'. Invariant: the card isn't Stealth Needle.
cardToInPlayCard :: Card -> InPlayCard
cardToInPlayCard = \case
  CardBase card ->
    InPlayCardBase (baseToInPlayBase card)

  CardShip card ->
    if card ^. the @"name" == "Stealth Needle"
      then error "cardToInPlayCard: Stealth Needle"
      else InPlayCardShip (shipToInPlayShip card)

  where
    baseToInPlayBase :: Base -> InPlayBase
    baseToInPlayBase base =
      InPlayBase
        { original = base
        , damage   = 0
        , primary  = Unplayed
        , ally     = Unplayed
        , scrap    = Unplayed
        }

    shipToInPlayShip :: Ship -> InPlayShip
    shipToInPlayShip ship =
      InPlayShip
        { original = ship
        , ally     = Unplayed
        , scrap    = Unplayed
        }
