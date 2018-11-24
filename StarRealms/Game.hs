module StarRealms.Game where

import StarRealms.Card
import StarRealms.Player

import qualified StarRealms.Deck as Deck

import Mitchell.Prelude

data Game
  = Game
  { player1      :: Player
  , player2      :: Player
  , tradeDeck    :: [Card]
  , scrapHeap    :: [Card]
  , tradeRow     :: [Card]
  } deriving stock (Generic)

-- | Move a card from the trade deck to the trade row.
drawCard :: Game -> Game
drawCard game =
  case Deck.drawCard (view (the @"tradeDeck") game) of
    Nothing ->
      game

    Just (card, cards) ->
      game
        & the @"tradeDeck" .~ cards
        & the @"tradeRow"  %~ (card:)
