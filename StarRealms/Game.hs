module StarRealms.Game where

import StarRealms.Card
import StarRealms.Player

data Game
  = Game
  { player1      :: Player
  , player2      :: Player
  , tradeDeck    :: [Card]
  , explorerDeck :: [Card]
  , scrapHeap    :: [Card]
  , tradeRow     :: [Card]
  }
