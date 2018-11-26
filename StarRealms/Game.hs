module StarRealms.Game where

import StarRealms.Card
import StarRealms.Card.Choice
import StarRealms.Game.Action
import StarRealms.Game.InPlayCard
import StarRealms.Player

import qualified StarRealms.Deck as Deck

import Mitchell.Prelude

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

-- | A lens onto the current player of a game.
currentPlayerL :: Lens' Game Player
currentPlayerL f game =
  case game ^. the @WhichPlayer of
    Player1 -> the @"player1" f game
    Player2 -> the @"player2" f game

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

updateGame :: (WhichPlayer, Action) -> Game -> UpdateGameResult
updateGame (player, action) game =
  case game ^. the @"state" of
    GameStateMain ->
      if player == game ^. the @WhichPlayer
        then case action of
          ActionPlayCard card choice ->
            case pluckCardFromHand card (game ^. currentPlayerL) of
              Nothing ->
                ActionInvalid

              Just (card', player') ->
                case cardPrimaryAbility (getCardByName card) of
                  Nothing ->
                    if choice == ChoiceNone
                      then ActionSuccessful
                        (let
                          inPlayCard :: InPlayCard
                          inPlayCard = cardToInPlayCard card'

                          newPlayer :: Player
                          newPlayer = (the @"inPlay" %~ (inPlayCard:)) player'

                          newGame :: Game
                          newGame = (currentPlayerL .~ newPlayer) game
                        in
                          newGame)
                      else ActionInvalid

                  Just ability -> undefined

          ActionUseAbilityOnCard card typ choice -> undefined
          ActionAssignCombat target -> undefined
          ActionPurchase card location -> undefined
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
