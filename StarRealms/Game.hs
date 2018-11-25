module StarRealms.Game where

import StarRealms.Card
import StarRealms.Player

import qualified StarRealms.Deck as Deck
import StarRealms.Game.Action
import StarRealms.Card.Choice

import Mitchell.Prelude
import Optic.Fold
import Alg.Functor

data Game
  = Game
  { player1      :: Player
  , player2      :: Player
  , tradeDeck    :: [Card]
  , scrapHeap    :: [Card]
  , tradeRow     :: [Card]
  } deriving stock (Generic)

gamePlayer :: WhichPlayer -> Lens' Game Player
gamePlayer = \case
  Player1 -> the @"player1"
  Player2 -> the @"player2"

data GameState
  = GameStateMain WhichPlayer
  | GameStateDiscard WhichPlayer
  | GameStateScrap WhichPlayer
  | GameStateMayDestroyBase WhichPlayer
  | GameStateDone

data UpdateGameResult
  = ActionInvalid
  | ActionSuccessful GameState Game

updateGame :: WhichPlayer -> Action -> GameState -> Game -> UpdateGameResult
updateGame player action state game =
  case state of
    GameStateMain player' ->
      if player == player'
        then case action of
          ActionPlayCardFromHand card choice ->
            let
              hand :: [Text]
              hand = toListOf
                ( gamePlayer player
                . the @"hand"
                . folded
                . cardName ) game
            in if card `elem` hand
              then
                case cardPrimaryAbility (getCardByName card) of
                  Nothing -> if choice == ChoiceNone
                    then ActionSuccessful state undefined
                    else ActionInvalid
                  Just ability -> undefined
              else ActionInvalid

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
