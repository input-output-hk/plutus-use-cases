{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Cardano.Crypto.Wallet (XPrv)
import Contracts.Oracle
import Data.Aeson
import Prelude
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let nargs = length args
  let keyBep = if nargs > 0 then args!!0  else ""
  let decodedPrvE = getXPrv keyBep
  decodePrv:: XPrv <- either (ioError . userError ) pure decodedPrvE
  let dtoEncoded = encodeKeyToDto decodePrv
  putStrLn "encoded prv:"
  putStrLn $ show dtoEncoded

  let params =  OracleParams
                { opFees =2_000_000
                , opSigner = dtoEncoded
                }
  putStrLn $ show $ Data.Aeson.encode params
