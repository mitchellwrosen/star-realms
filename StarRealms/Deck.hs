module StarRealms.Deck where

import StarRealms.Card

import Mitchell.Prelude

drawCard :: [Card] -> Maybe (Card, [Card])
drawCard = \case
  [] -> Nothing
  x:xs -> Just (x, xs)

-- Remove a card from a deck by name. If it doesn't exist in the deck, return
-- Nothing.
removeCard :: Text -> [Card] -> Maybe [Card]
removeCard name = \case
  [] ->
    Nothing

  card:cards
    | view cardName card == name ->
        Just cards

    | otherwise ->
        (card:) <$> removeCard name cards
