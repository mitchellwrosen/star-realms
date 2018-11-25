module StarRealms.Deck where

import StarRealms.Card

import Mitchell.Prelude

-- Draw a card from the top of a deck. If the deck is empty, return Nothing.
drawCard :: [Card] -> Maybe (Card, [Card])
drawCard = \case
  [] -> Nothing
  x:xs -> Just (x, xs)

-- Remove a card from a deck by name, and return it. If it doesn't exist in the
-- deck, return Nothing.
pluckCard :: Text -> [Card] -> Maybe (Card, [Card])
pluckCard name = \case
  [] ->
    Nothing

  card:cards
    | view cardName card == name ->
        Just (card, cards)

    | otherwise ->
        over (_Just . _2) (card:) (pluckCard name cards)
