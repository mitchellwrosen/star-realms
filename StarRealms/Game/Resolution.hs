-- | The resolution of an ability.

module StarRealms.Game.Resolution
  ( Resolution(..)
  ) where

import StarRealms.Card.Ability
import StarRealms.Card.Choice
import StarRealms.Condition
import StarRealms.Location

import Mitchell.Prelude
import Num.Natural

data Resolution
  = ResolutionNone
  | ResolutionAcquire Text
  | ResolutionAnd Resolution Resolution
  | ResolutionAuthority Natural
  | ResolutionScrap [(Text, Location)]
  | ResolutionCombat Natural
  | ResolutionDestroyBase Text
  | ResolutionDiscard [Text]
  | ResolutionDraw Natural
  | ResolutionNextShipOnTop
  | ResolutionOpponentDiscards
  | ResolutionStealthNeedle Text Choice
  | ResolutionTrade Natural

resolve
  :: Ability
  -> Choice
  -> (Condition -> Bool)
  -> (GameNatural -> Natural)
  -> Maybe Resolution
resolve ability choice f g =
  case ability of
    AbilityAcquire ->
      case choice of
        ChoiceCard card -> Just (ResolutionAcquire card)
        _ -> Nothing

    AbilityAnd ab1 ab2 ->
      case choice of
        ChoiceAnd c1 c2 ->
          liftA2 ResolutionAnd (resolve ab1 c1 f g) (resolve ab2 c2 f g)
        _ -> Nothing

    AbilityAndOr ab1 ab2 ->
      case choice of
        ChoiceNone -> Just ResolutionNone
        ChoiceLeft c1 -> resolve ab1 c1 f g
        ChoiceRight c2 -> resolve ab2 c2 f g
        ChoiceAnd c1 c2 ->
          liftA2 ResolutionAnd (resolve ab1 c1 f g) (resolve ab2 c2 f g)
        _ -> Nothing

    AbilityAuthority x ->
      case choice of
        ChoiceNone -> Just (ResolutionAuthority x)
        _ -> Nothing

    AbilityCombat x ->
      case choice of
        ChoiceNone -> Just (ResolutionCombat x)
        _ -> Nothing

    AbilityConditional con ab ->
      if f con
        then case choice of
          ChoiceNone -> Just ResolutionNone
          _ -> Nothing
        else resolve ab choice f g

    AbilityDestroyBase ->
      case choice of
        ChoiceCard card -> Just (ResolutionDestroyBase card)
        _ -> Nothing

    AbilityDiscardThenDraw x ->
      case choice of
        ChoiceCards cards ->
          let y = fromIntegral (length cards) in
            if y <= x
              then Just (ResolutionDraw y)
              else Nothing
        _ -> Nothing

    AbilityDraw x ->
        case choice of
          ChoiceNone -> Just (ResolutionDraw (g x))
          _ -> Nothing

    AbilityMay ab ->
      case choice of
        ChoiceYes c -> resolve ab c f g
        _ -> Nothing

    AbilityNextShipOnTop ->
      case choice of
        ChoiceNone -> Just ResolutionNextShipOnTop
        _ -> Nothing

    AbilityOpponentDiscards ->
      case choice of
        ChoiceNone -> Just ResolutionOpponentDiscards
        _ -> Nothing

    AbilityOr ab1 ab2 ->
      case choice of
        ChoiceNone -> Just ResolutionNone
        ChoiceLeft c1 -> resolve ab1 c1 f g
        ChoiceRight c2 -> resolve ab2 c2 f g
        _ -> Nothing

    AbilityScrap locations ->
      case choice of
        ChoiceCardsIn xs ->
          if all (\(_, loc) -> loc `elem` locations) xs
              then Just (ResolutionScrap xs)
              else Nothing
        _ -> Nothing

    AbilityStealthNeedle ->
      case choice of
        ChoiceAnd (ChoiceCard ship) c -> Just (ResolutionStealthNeedle ship c)
        _ -> Nothing

    AbilityTrade x ->
      case choice of
        ChoiceNone -> Just (ResolutionTrade x)
        _ -> Nothing
