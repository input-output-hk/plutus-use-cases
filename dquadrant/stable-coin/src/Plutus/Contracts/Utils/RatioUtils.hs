-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DerivingVia #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE MonoLocalBinds #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE ViewPatterns #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
-- {-# OPTIONS_GHC -Wno-name-shadowing #-}
-- {-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- {-# OPTIONS_GHC -fno-strictness #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context #-}

module Plutus.Contracts.Utils.RatioUtils
--   ( Ratio(..),
--     fromInteger,
--     multiply,
--     round,
--     zero,
--     toDouble,
--     geq,
--     leq
--   )
where

-- import Data.Aeson (FromJSON, ToJSON)
-- import Data.OpenApi.Schema as OpenApi
-- import GHC.Generics (Generic)
-- import qualified PlutusTx as PlutusTx
-- import PlutusTx.Prelude hiding (fromInteger, round, zero)
-- import qualified Prelude
-- import qualified PlutusTx.Ratio as R

-- data Ratio = Ratio Integer Integer
--   deriving stock (Prelude.Eq, Prelude.Ord, Prelude.Show, Generic)
--   deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)


-- --TODO Here use Plutus Tx.Ratio
-- fromInteger :: Integer -> Ratio
-- fromInteger x = Ratio x 1

-- multiply :: Ratio -> Ratio -> Ratio
-- multiply (Ratio n1 d1) (Ratio n2 d2) = Ratio (n1 * n2) (d1 * d2)

-- toDouble :: Integer -> Prelude.Double
-- toDouble x = Prelude.fromIntegral x

-- round :: Ratio -> Integer
-- round (Ratio n d) = R.round $ n R.% d
    
-- zero :: Ratio
-- zero = Ratio 0 1

-- geq :: Ratio -> Ratio -> Bool
-- geq (Ratio n1 d1) (Ratio n2 d2) = n1 R.% d1 >= n2 R.% d2

-- leq :: Ratio -> Ratio -> Bool
-- leq (Ratio n1 d1) (Ratio n2 d2) = n1 R.% d1 <= n2 R.% d2