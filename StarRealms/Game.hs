module StarRealms.Game where

import StarRealms.Card
import StarRealms.Player

import           StarRealms.Card.Choice
import qualified StarRealms.Deck        as Deck
import           StarRealms.Game.Action

import Alg.Functor
import Mitchell.Prelude
import Optic.Fold

data Game
  = Game
  { state     :: GameState
  , player    :: WhichPlayer
  , player1   :: Player
  , player2   :: Player
  , tradeDeck :: [Card]
  , scrapHeap :: [Card]
  , tradeRow  :: [Card]
  } deriving stock (Generic)

-- | Get the current player of a game.
getCurrentPlayer :: Game -> Player
getCurrentPlayer game =
  case game ^. the @WhichPlayer of
    Player1 -> game ^. the @"player1"
    Player2 -> game ^. the @"player2"

-- | The state of the game, which determines which messages are valid.
data GameState
  -- | The "main" state.
  = GameStateMain
  -- | The "discard" state. We are waiting for the opponent to discard a card.
  | GameStateDiscard
  -- | The "scrap" state. We are waiting for the current player to decide what
  -- card to discard from their hand.
  | GameStateScrap
  -- | The "may destroy base" state. We are waiting for the current player to
  -- decide if they want to destroy a base.
  | GameStateMayDestroyBase
  | GameStateDone

data UpdateGameResult
  = ActionInvalid
  | ActionSuccessful Game

updateGame :: WhichPlayer -> Action -> Game -> UpdateGameResult
updateGame player action game =
  case game ^. the @"state" of
    GameStateMain ->
      if player == game ^. the @WhichPlayer
        then case action of
          ActionPlayCardFromHand card choice ->
            case pluckCardFromHand card (getCurrentPlayer game) of
              Nothing -> ActionInvalid
              Just (card', player'') ->
                case cardPrimaryAbility (getCardByName card) of
                  Nothing ->
                    if choice == ChoiceNone
                      then ActionSuccessful undefined
                      else ActionInvalid
                  Just ability -> undefined

          ActionUseAbilityOnCard card typ choice -> undefined
          ActionCombat target -> undefined
          ActionPurchaseTradeRow card location -> undefined
          ActionPurchaseExplorer location -> undefined
          ActionDiscard card -> ActionInvalid
          ActionScrap card -> ActionInvalid
          ActionEndTurn -> undefined
        else ActionInvalid

-- abilityHasChoice :: Ability -> Choice -> something?
abilityHasChoice = undefined

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
