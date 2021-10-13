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
{-# LANGUAGE TemplateHaskell    #-}

module Types.Game
     where

import           Control.Lens    
import           Data.Map             (fromList, lookup)
import qualified Data.Map   as Map 
import           Data.Maybe           (fromMaybe)
import           Data.Aeson
import           Data.Aeson.TH        
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Schema               (ToSchema)

type GameId = Integer
type TeamId = Integer
type Goal   = Integer

skipUnderscore:: String -> String
skipUnderscore = drop 1

renameLabel:: String -> String -> String -> String
renameLabel toRenameLabel targetLabel value = if value == toRenameLabel then targetLabel else value

data Team = Team 
    { _teamId :: !TeamId
    , _name   :: !Text
    , _logo   :: !Text
    , _winner :: !Bool
    }  deriving (Show,Generic)
instance FromJSON Team where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = renameLabel "teamId" "id" . skipUnderscore  }
instance ToJSON Team where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = renameLabel "teamId" "id" . skipUnderscore   }
makeLenses ''Team

data GameTeams = GameTeams 
    { _home :: !Team
    , _away :: !Team
    }  deriving (Show,Generic)
instance FromJSON GameTeams where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = skipUnderscore}
instance ToJSON GameTeams where 
   toJSON = genericToJSON defaultOptions{fieldLabelModifier = skipUnderscore}
makeLenses ''GameTeams

data GameGoals = GameGoals 
    { _teamHome :: !Goal
    , _teamAway :: !Goal
    }  deriving (Show,Generic)
instance FromJSON GameGoals where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = renameLabel "teamHome" "home" . renameLabel "teamAway" "away" . skipUnderscore}
instance ToJSON GameGoals where 
   toJSON = genericToJSON defaultOptions{fieldLabelModifier = renameLabel "teamHome" "home" . renameLabel "teamAway" "away" . skipUnderscore}
makeLenses ''GameGoals

data FixtureStatusShort = NS | LIVE | FT | CANC
    deriving (Generic, Show, Enum, Eq, Ord, ToSchema)
instance FromJSON FixtureStatusShort
instance ToJSON FixtureStatusShort 

fixureStatusLong :: Map.Map FixtureStatusShort Text
fixureStatusLong = fromList [(NS,"Not Started"), (LIVE,"In Progress"), (FT, "Match Finished"), (CANC, "Match Cancelled")]

createFixtureStatus :: FixtureStatusShort -> FixtureStatus 
createFixtureStatus status = FixtureStatus
        {_short = status
        , _long = fromMaybe "" $ Map.lookup status fixureStatusLong
        } 
data FixtureStatus = FixtureStatus 
    { _long    :: !Text
    , _short   :: !FixtureStatusShort
    } deriving (Show,Generic)
instance FromJSON FixtureStatus where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = skipUnderscore}
instance ToJSON FixtureStatus where 
   toJSON = genericToJSON defaultOptions{fieldLabelModifier = skipUnderscore}
makeLenses ''FixtureStatus

data FixtureVenue = FixtureVenue 
    { _venueName  :: !Text
    , _city       :: !Text
    } deriving (Show,Generic)
instance FromJSON FixtureVenue where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = renameLabel "venueName" "name" . skipUnderscore}
instance ToJSON FixtureVenue where 
   toJSON = genericToJSON defaultOptions{fieldLabelModifier = renameLabel "venueName" "name" . skipUnderscore}
makeLenses ''FixtureVenue

data Fixture = Fixture
    { _fixtureId :: !GameId
    , _referee   :: !Text
    , _timezone  :: !Text
    , _date      :: !Text  
    , _status    :: !FixtureStatus
    , _venue     :: !FixtureVenue
    } deriving (Show,Generic)
instance FromJSON Fixture where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = renameLabel "fixtureId" "id" . skipUnderscore }
instance ToJSON Fixture where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = renameLabel "fixtureId" "id" . skipUnderscore   }
makeLenses ''Fixture

data Game = Game  
    { _fixture :: !Fixture
    , _teams   :: !GameTeams
    , _goals   :: !GameGoals
    } 
    deriving  (Show, Generic)
makeLenses ''Game
instance FromJSON Game where
    parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = skipUnderscore}
instance ToJSON Game where 
   toJSON = genericToJSON defaultOptions{fieldLabelModifier = skipUnderscore}
instance Eq Game where
    a == b = (a ^. fixture . fixtureId == b ^. fixture . fixtureId)

getWinnerTeamId :: Game -> Either String Integer
getWinnerTeamId game =
    if game ^. fixture . status . short /= FT
        then Left "Game not finished"
        else do
            let team1 = game ^. teams . home
            let team2 = game ^. teams . away
            if (team1 ^. winner) 
                then Right (team1 ^. teamId ) 
                else Right (team2 ^. teamId) 

isGameClosed :: FixtureStatusShort -> Bool
isGameClosed FT = True

{-# INLINABLE validateGameStatusChanges #-}
validateGameStatusChanges:: FixtureStatusShort -> FixtureStatusShort -> Bool
validateGameStatusChanges NS LIVE = True
validateGameStatusChanges NS CANC = True
validateGameStatusChanges LIVE FT = True
validateGameStatusChanges _ _     = False