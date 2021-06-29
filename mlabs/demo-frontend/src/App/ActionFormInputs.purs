module App.ActionFormInputs (actionFormInputs) where

import Prelude hiding (div)
import Bootstrap as BS
import Bootstrap as Bootstrap
import Data.Array as Array
-- import Data.BigInteger as BigInteger
import Data.BigInt as BigInt
-- import Data.Functor.Foldable (Fix(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Tuple (Tuple(..), fst)
import Data.Lens (Lens', over, set, view)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as Keyed
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (IProp, InputType(..), checked, class_, classes, for, id_, name, placeholder, required, type_, value)
import Halogen.HTML.Properties as HP
-- import Icons (Icon(..), icon)
-- import Ledger.Extra (_LowerBoundExtended, _LowerBoundInclusive, _UpperBoundExtended, _UpperBoundInclusive, _ivFrom, _ivTo, humaniseSlotInterval, humaniseTimeInterval)
-- import Plutus.V1.Ledger.Interval (Extended(..), Interval, _Interval)
-- import Plutus.V1.Ledger.Slot (Slot(..))
-- import Plutus.V1.Ledger.Time (POSIXTime(..))
import Prim.TypeError (class Warn, Text)
-- import Schema (FormArgumentF(..))
-- import Schema.Types (FieldEvent(..), FormArgument, FormEvent(..))
-- import Validation (ValidationError, WithPath, joinPath, showPathValue, validate)
-- import ValueEditor (valueForm)

import App.Types (Action(..), FieldEvent(..))
import Data.Json.JsonTuple (JsonTuple(..))
import PAB.AssocMap as AssocMap
import PAB.Types (CurrencySymbol(..), Fix(..), FormArgument, FormArgumentF(..), Interval(..), TokenName(..), Value(..))


actionFormInputs :: forall p. FormArgument -> HH.HTML p Action
actionFormInputs arg =
  HH.div 
  [ classes [ BS.wasValidated, BS.mb3 ] ]
  [ actionArgumentField [] false arg ]

actionArgumentField :: forall p. Array String -> Boolean -> FormArgument -> HH.HTML p Action
actionArgumentField ancestors isNested (Fix (FormObjectF subFields)) =
  HH.div [ HP.classes $ defaultClasses isNested ]
    (mapWithIndex (\i (JsonTuple field) -> subForm i field) subFields)
 where
  subForm index (name /\ arg) =
    ( BS.formGroup_
        [ HH.label [ for name ] [ HH.text name ]
        , actionArgumentField (Array.snoc ancestors name) true arg
        ]
    )

actionArgumentField ancestors isNested arg@(Fix FormUnitF) = BS.empty

actionArgumentField ancestors isNested arg@(Fix (FormIntF n)) =
  HH.div
    [ HP.classes $ defaultClasses isNested ]
    [ HH.input
        [ type_ InputNumber
        , classes [ BS.formControl ]
        , value $ maybe "" show n
        , required true
        , placeholder "Int"
        , HE.onValueInput (SetField <<< SetIntField <<< Int.fromString)
        ]
    -- , validationFeedback (joinPath ancestors <$> validate arg)
    ]

actionArgumentField ancestors _ arg@(Fix (FormBoolF b)) =
  HH.div 
    [ HP.classes $ defaultClasses false <> [ BS.formCheck ] ]
    [ HH.input
        [ type_ InputCheckbox
        , id_ elementId
        , classes (Array.cons BS.formCheckInput (actionArgumentClass ancestors))
        , checked b
        , HE.onChecked (SetField <<< SetBoolField)
        ]
    , HH.label
        [ class_ BS.formCheckLabel
        , for elementId
        ]
        [ HH.text (if b then "True" else "False") ]
    -- , validationFeedback (joinPath ancestors <$> validate arg)
    ]
  where
  elementId = String.joinWith "-" ancestors

actionArgumentField ancestors isNested arg@(Fix (FormIntegerF n)) =
  HH.div
    [ HP.classes $ defaultClasses false ]
    [ HH.input
        [ type_ InputNumber
        , classes (Array.cons BS.formControl (actionArgumentClass ancestors))
        , value $ maybe "" show n
        , required true
        , placeholder "Integer"
        , HE.onValueInput (SetField <<< SetBigIntegerField <<< Int.fromString)
        ]
    -- , validationFeedback (joinPath ancestors <$> validate arg)
    ]

actionArgumentField ancestors _ arg@(Fix (FormStringF s)) =
  HH.div
    [ HP.classes $ defaultClasses false ]
    [ HH.input
        [ type_ InputText
        , classes (Array.cons BS.formControl (actionArgumentClass ancestors))
        , value $ fromMaybe "" s
        -- empty text HH.inputs give `Just ""` as a value, which might be wanted,
        -- so don't mark these fields as required
        , required false
        , placeholder "String"
        , HE.onValueInput (SetField <<< SetStringField)
        ]
    -- , validationFeedback (joinPath ancestors <$> validate arg)
    ]

actionArgumentField ancestors _ arg@(Fix (FormRadioF options s)) =
  HH.div
    [ HP.classes $ defaultClasses false <> [ BS.formGroup ]]
    [ HH.div_ (radioItem <$> options)
    -- , validationFeedback (joinPath ancestors <$> validate arg)
    ]
  where
  radioItem :: String -> HH.HTML p Action
  radioItem option =
    let
      elementId = String.joinWith "-" (ancestors <> [ option ])
    in
      BS.formCheck_
        [ HH.input
            [ type_ InputRadio
            , id_ elementId
            , classes (Array.cons BS.formCheckInput (actionArgumentClass ancestors))
            , name option
            , value option
            , required (s == Nothing)
            , HE.onValueInput (SetField <<< SetRadioField)
            , checked (Just option == s)
            ]
        , HH.label
            [ class_ BS.formCheckLabel
            , for elementId
            ]
            [ HH.text option ]
        ]

actionArgumentField ancestors _ arg@(Fix (FormHexF s)) =
  HH.div
    [ HP.classes $ defaultClasses false ]
    [ HH.input
        [ type_ InputText
        , classes (Array.cons BS.formControl (actionArgumentClass ancestors))
        , value $ fromMaybe "" s
        , required true
        , placeholder "String"
        , HE.onValueInput (SetField <<< SetHexField)
        ]
    -- , validationFeedback (joinPath ancestors <$> validate arg)
    ]

actionArgumentField ancestors isNested (Fix (FormTupleF subFieldA subFieldB)) =
  HH.div
    [ HP.classes $ defaultClasses isNested ]
    [ BS.formGroup_
        [ SetSubField 1 <$> actionArgumentField (Array.snoc ancestors "_1") true subFieldA ]
    , BS.formGroup_
        [ SetSubField 2 <$> actionArgumentField (Array.snoc ancestors "_2") true subFieldB ]
    ]

-- actionArgumentField ancestors isNested (Fix (FormArrayF schema subFields)) =
--   HH.div_
--     [ Keyed.div [ nesting isNested ]
--         (mapWithIndex subFormContainer subFields)
--     -- , HH.button
--     --     [ classes [ BS.btn, BS.btnInfo ]
--     --     , HE.onClick $ const $ Just AddSubField
--     --     ]
--     --     [ icon Plus ]
--     ]
--   where
--   subFormContainer i field =
--     show i
--       /\ BS.formGroup_
--           [ BS.row_
--               [ BS.col10_
--                   [ SetSubField i <$> actionArgumentField (Array.snoc ancestors (show i)) true field ]
--               -- , BS.col2_
--               --     [ HH.button
--               --         [ classes [ BS.btn, BS.btnLink ]
--               --         , HE.onClick $ const $ Just (RemoveSubField i)
--               --         ]
--               --         [ icon Trash ]
--               --     ]
--               ]
--           ]

-- actionArgumentField ancestors isNested (Fix (FormPOSIXTimeRangeF interval)) =
--   div [ class_ formGroup, nesting isNested ]
--     [ label [ for "interval" ] [ text "Interval" ]
--     , formRow_
--         [ label [ for "ivFrom", classes [ col, colFormLabel ] ] [ text "From" ]
--         , label [ for "ivTo", classes [ col, colFormLabel ] ] [ text "To" ]
--         ]
--     , formRow_
--         [ let
--             extensionLens :: Lens' (Interval POSIXTime) (Extended POSIXTime)
--             extensionLens = _Interval <<< _ivFrom <<< _LowerBoundExtended

--             inclusionLens :: Lens' (Interval POSIXTime) Boolean
--             inclusionLens = _Interval <<< _ivFrom <<< _LowerBoundInclusive
--           in
--             div [ classes [ col, extentFieldClass ] ]
--               [ HH.inputGroup_
--                   [ HH.inputGroupPrepend_
--                       [ extentFieldExtendedButton extensionLens NegInf
--                       , extentFieldInclusionButton inclusionLens StepBackward Reverse
--                       ]
--                   , extentFieldInput extensionLens
--                   ]
--               ]
--         , let
--             extensionLens :: Lens' (Interval POSIXTime) (Extended POSIXTime)
--             extensionLens = _Interval <<< _ivTo <<< _UpperBoundExtended

--             inclusionLens :: Lens' (Interval POSIXTime) Boolean
--             inclusionLens = _Interval <<< _ivTo <<< _UpperBoundInclusive
--           in
--             div [ classes [ col, extentFieldClass ] ]
--               [ HH.inputGroup_
--                   [ extentFieldInput extensionLens
--                   , HH.inputGroupAppend_
--                       [ extentFieldInclusionButton inclusionLens StepForward Play
--                       , extentFieldExtendedButton extensionLens PosInf
--                       ]
--                   ]
--               ]
--         ]
--     , small
--         [ classes [ formText, textMuted ] ]
--         [ text $ humaniseTimeInterval interval
--         ]
--     ]
--   where
--   extentFieldClass = H.ClassName "extent-field"

--   extentFieldInclusionButton :: Lens' (Interval Int) Boolean -> Icon -> Icon -> HTML p FormEvent
--   extentFieldInclusionButton inclusionLens inclusionIcon exclusionIcon =
--     button
--       [ classes [ btn, btnSmall, btnPrimary ]
--       , onClick $ const $ Just $ SetField $ SetPOSIXTimeRangeField $ over inclusionLens not interval
--       ]
--       [ icon
--           $ if view inclusionLens interval then
--               inclusionIcon
--             else
--               exclusionIcon
--       ]

--   extentFieldExtendedButton :: Lens' (Interval POSIXTime) (Extended POSIXTime) -> Extended POSIXTime -> HTML p FormEvent
--   extentFieldExtendedButton extensionLens value =
--     button
--       [ classes
--           [ btn
--           , btnSmall
--           , if view extensionLens interval == value then
--               btnPrimary
--             else
--               btnInfo
--           ]
--       , onClick $ const $ Just $ SetField $ SetPOSIXTimeRangeField $ set extensionLens value interval
--       ]
--       [ icon Infinity ]

--   extentFieldInput :: Lens' (Interval POSIXTime) (Extended POSIXTime) -> HTML p FormEvent
--   extentFieldInput extensionLens =
--     HH.input
--       [ type_ HH.inputNumber
--       , class_ formControl
--       , HP.min zero
--       , value
--           $ case view extensionLens interval of
--               Finite (POSIXTime time) -> show time.getPOSIXTime
--               _ -> mempty
--       , onValueInput $ map (\n -> SetField (SetPOSIXTimeRangeField (set extensionLens (Finite (POSIXTime { getPOSIXTime: n })) interval))) <<< BigInteger.fromString
--       ]

-- actionArgumentField ancestors isNested (Fix (FormValueF value)) =
--   HH.div
--     [ nesting isNested ]
--     [ BS.valueForm (SetField <<< SetValueField) value ]

actionArgumentField _ _ (Fix (FormMaybeF dataType child)) =
  HH.div
    [ HP.classes $ defaultClasses false ]
    [ HH.text "Unsupported Maybe"
    , HH.code_ [ HH.text $ show dataType ]
    -- , HH.code_ [ HH.text $ show child ]
    , HH.code_ [ HH.text $ show child ]
    ]

actionArgumentField _ _ (Fix (FormUnsupportedF description)) =
  HH.div
    [ HP.classes $ defaultClasses false ]
    [ HH.code_ [ HH.text description ]
    ]

actionArgumentField _ _ _ =
  HH.div_
    [ HH.text "TODO: Support this data type"
    ]

actionArgumentClass :: Array String -> Array H.ClassName
actionArgumentClass ancestors =
  [ H.ClassName "action-argument"
  , H.ClassName $ "action-argument-" <> Array.intercalate "-" ancestors
  ]

-- validationFeedback :: forall p i. Array (WithPath ValidationError) -> HTML p i
-- validationFeedback [] = validFeedback_ []

-- validationFeedback errors = invalidFeedback_ (div_ <<< pure <<< text <<< showPathValue <$> errors)

nested :: H.ClassName
nested = H.ClassName "nested"

defaultClasses :: forall r i. Boolean -> Array H.ClassName
defaultClasses isNested = 
  case isNested of
    true  -> [ BS.mb3, nested ]
    false -> [ BS.mb3 ]

-- valueForm :: forall p i. (ValueEvent -> i) -> Value -> HH.HTML p i
-- valueForm handler ({ getValue: balances }) =
--   Keyed.div_
--     (Array.concat (mapWithIndex (currencyRow handler) (Array.sortWith fst $ AssocMap.toTuples balances)))

-- currencyRow ::
--   forall p i.
--   (Action -> i) ->
--   Int ->
--   Tuple CurrencySymbol (Map TokenName BigInt.BigInt) ->
--   Array (Tuple String (HH.HTML p i))
-- currencyRow handler currencyIndex (Tuple currencySymbol tokenBalances) = mapWithIndex (balanceRow handler currencyIndex currencySymbol) (Array.sortWith fst $ AssocMap.toTuples tokenBalances)


-- balanceRow ::
--   forall p i.
--   (Action -> i) ->
--   Int ->
--   CurrencySymbol ->
--   Int ->
--   Tuple TokenName BigInt.BigInt ->
--   Tuple String (HH.HTML p i)
-- balanceRow handler currencyIndex currencySymbol tokenIndex (Tuple tokenName amount) =
--   (show currencyIndex <> "-" <> show tokenIndex)
--     /\ HH.div
--         [ classes
--             [ BS.formGroup
--             , H.ClassName "balance"
--             , H.ClassName ("balance-" <> show currencyIndex <> show tokenIndex)
--             ]
--         ]
--         [ BS.formRow_
--             $ [ HH.label
--                   [ classes [ BS.col, BS.colFormLabel ] ]
--                   [ HH.text
--                       $ case currencySymbol.unCurrencySymbol, tokenName.unTokenName of
--                           "", "" -> "Lovelace"
--                           _, other -> other
--                   ]
--               , BS.col_
--                   [ HH.input
--                       [ type_ InputNumber
--                       , classes [ BS.formControl, H.ClassName "balance-amount" ]
--                       , value $ show amount
--                       , required true
--                       , placeholder "Amount"
--                       , HP.min zero
--                       , HE.onValueInput
--                           $ \str ->
--                               -- default to 0 in case of empty or invalid input
--                               -- (for reasons I have yet to fathom, this doesn't work when you delete "0";
--                               -- until I get to the bottom of that, this is at least an improvement)
--                               let
--                                 newAmount = fromMaybe zero $ BigInt.fromString str
--                               in
--                                 do
--                                   pure $ handler $ SetBalance currencySymbol tokenName newAmount
--                       ]
--                   ]
--               ]
--         ]
