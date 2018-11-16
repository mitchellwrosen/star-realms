module StarRealms.Deck where

import StarRealms.Card

import Mitchell.Prelude

-- Remove a card from a deck by name. If it doesn't exist in the deck, return
-- Nothing.
removeCard :: Text -> [Card] -> Maybe [Card]
removeCard name = \case
  [] ->
    Nothing

  card:cards ->
    undefined
