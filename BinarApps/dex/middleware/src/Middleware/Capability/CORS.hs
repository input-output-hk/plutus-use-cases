{-# LANGUAGE OverloadedStrings #-}
-- | CORS config

module Middleware.Capability.CORS (corsConfig) where

import Network.Wai                 (Middleware)
import Network.Wai.Middleware.Cors

corsPolicy :: CorsResourcePolicy
corsPolicy =
  CorsResourcePolicy {
    corsOrigins = Nothing,
    corsMethods = methods,
    corsRequestHeaders = ["Content-Type"],
    corsExposedHeaders = Nothing,
    corsMaxAge = Nothing,
    corsVaryOrigin = True,
    corsRequireOrigin = False,
    corsIgnoreFailures = False
  }
  where
    methods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    cont = simpleContentTypes <> ["application/json"]

corsConfig :: Middleware
corsConfig = cors (const $ Just corsPolicy)
