{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Pab.Game
    ( getGames
    , Game (..)
    , Fixture (..)
    , FixtureStatus (..)
    , GameTeams (..)
    , Team(..)
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Default           (Default (def))
import Data.Text              (Text, pack)
import GHC.Generics           (Generic)


getGames :: IO (Either String [Game])
getGames = do
    games <- (eitherDecodeFileStrict "fixture.json" :: IO (Either String [Game]))
    return games

data Team = Team 
    { teamId :: !Integer
    , name   :: !Text
    , logo   :: !Text
    , winner :: !Bool
    }  deriving (Show,Generic)
instance FromJSON Team where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \f -> if f == "teamId" then "id" else f  }

instance ToJSON Team where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = \f -> if f == "id" then "teamId" else f  }

data GameTeams = GameTeams 
    { home :: !Team
    , away :: !Team
    }  deriving (Show,Generic)
instance FromJSON GameTeams
instance ToJSON GameTeams

data FixtureStatus = FixtureStatus 
    { long    :: !Text
    , short   :: !Text
    , elapsed :: !Integer
    } deriving (Show,Generic)
instance FromJSON FixtureStatus
instance ToJSON FixtureStatus

data Fixture = Fixture
    { fixtureId :: !Integer
    , referee   :: !Text
    , timezone  :: !Text
    , date      :: !Text  
    , status    :: !FixtureStatus
    } deriving (Show,Generic)
instance FromJSON Fixture where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = \f -> if f == "fixtureId" then "id" else f  }
instance ToJSON Fixture where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = \f -> if f == "id" then "fixtureId" else f  }

data Game = Game  
    { fixture :: !Fixture
    , teams   :: !GameTeams
    } deriving (Show,Generic)
instance FromJSON Game
instance ToJSON Game
