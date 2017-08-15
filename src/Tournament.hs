{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Tournament (serve, runDB) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text, unpack)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), object, withObject, (.=), (.:))
import Database.Persist.TH
import Database.Persist ((+=.), (<-.), (==.))
import qualified Database.Persist.Sqlite as DB
import qualified Web.Scotty as Scotty
import qualified Network.HTTP.Types.Status as HTTPStatus

-- Database model

type Query a = DB.SqlPersistT (NoLoggingT (ResourceT IO)) a

type Balance = Double

share [ mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"
      ] [persistLowerCase|
Player
    Id String
    balance Balance

Tournament
    Id String
    deposit Balance

Participation
    tournamentId TournamentId
    playerId PlayerId
    backersIds [PlayerId]
|]

instance ToJSON (DB.Entity Player) where
    toJSON (DB.Entity playerId (Player balance)) =
        object [ "playerId" .= playerId
               , "balance" .= balance
               ]


-- Result Model

data Winner = Winner { winnerPlayerId :: PlayerId
                     , winnerPrize :: Balance
                     } deriving (Show)

data Result = Result { resultTournamentId :: TournamentId
                     , resultWinners :: [Winner]
                     } deriving (Show)

instance FromJSON Winner where
    parseJSON = withObject "Winner" $ \value -> Winner
        <$> value .: "playerId"
        <*> value .: "prize"

instance FromJSON Result where
    parseJSON = withObject "Result" $ \value -> Result
        <$> (TournamentKey <$> value .: "tournamentId")
        <*> value .: "winners"


-- Helpers

runDB :: Query a -> IO a
runDB = DB.runSqlite "db.sqlite3"

migrate :: IO ()
migrate = runDB $ DB.runMigration migrateAll

updatePlayerBalance :: Balance -> PlayerId -> IO ()
updatePlayerBalance delta playerId = runDB $ do
    maybePlayer <- DB.get playerId
    case maybePlayer of
        Just _ ->
            DB.update playerId [PlayerBalance +=. delta]
        Nothing ->
            DB.insertKey playerId $ Player delta

updatePlayersBalanciesPortional' :: Balance -> [PlayerId] -> Query ()
updatePlayersBalanciesPortional' delta playersIds = do
    let personalDelta = delta / (fromIntegral $ length playersIds)
    DB.updateWhere [PlayerId <-. playersIds] [PlayerBalance +=. personalDelta]

-- Get array of all values for query param
getMultipleStringParam :: Text -> Scotty.ActionM [String]
getMultipleStringParam key = do
    params <- Scotty.params
    return $ map (unpack . snd)
           $ filter ((== key) . fst)
           $ params


-- Take points from player/Fund points to player

handleFundPointsToPlayer :: Scotty.ActionM ()
handleFundPointsToPlayer = do
    points <- Scotty.param "points"
    playerId <- PlayerKey <$> Scotty.param "playerId"
    liftIO $ updatePlayerBalance points playerId

handleTakePointsFromPlayer :: Scotty.ActionM ()
handleTakePointsFromPlayer = do
    points <- Scotty.param "points"
    playerId <- PlayerKey <$> Scotty.param "playerId"
    liftIO $ updatePlayerBalance (- points) playerId


-- Reset

dropDB :: IO ()
dropDB = runDB $ do
    DB.deleteWhere ([] :: [DB.Filter Participation])
    DB.deleteWhere ([] :: [DB.Filter Player])
    DB.deleteWhere ([] :: [DB.Filter Tournament])

handleReset :: Scotty.ActionM ()
handleReset = liftIO dropDB


-- Announce tournament

createTournament :: TournamentId -> Balance -> IO ()
createTournament tournamentId deposit = runDB $ do
    DB.deleteCascade tournamentId
    DB.insertKey tournamentId (Tournament deposit)

handleAnnounceTournament :: Scotty.ActionM ()
handleAnnounceTournament = do
    tournamentId <- TournamentKey <$> Scotty.param "tournamentId"
    deposit <- Scotty.param "deposit"
    liftIO $ createTournament tournamentId deposit


-- Get balance

getPlayer :: PlayerId -> IO (Maybe (DB.Entity Player))
getPlayer playerId = runDB $ DB.getEntity playerId

handleGetBalance :: Scotty.ActionM ()
handleGetBalance = do
    playerId <- PlayerKey <$> Scotty.param "playerId"
    maybePlayer <- liftIO $ getPlayer playerId
    case maybePlayer of
        Just player ->
            Scotty.json player
        Nothing ->
            Scotty.status HTTPStatus.badRequest400


-- Join tournament

createParticipation' :: TournamentId -> PlayerId -> [PlayerId] -> Query ()
createParticipation' tournamentId playerId backersIds = do
    DB.insert_ $ Participation tournamentId playerId backersIds

takeDistributedDeposit' :: TournamentId -> [PlayerId] -> Query ()
takeDistributedDeposit' tournamentId playersIds = do
    Tournament deposit <- DB.getJust tournamentId
    updatePlayersBalanciesPortional' (- deposit) playersIds

handleJoinTournament :: Scotty.ActionM ()
handleJoinTournament = do
    tournamentId <- TournamentKey <$> Scotty.param "tournamentId"
    playerId <- PlayerKey <$> Scotty.param "playerId"
    backersIds <- map PlayerKey <$> getMultipleStringParam "backerId"
    liftIO $ runDB $ do
        createParticipation' tournamentId playerId backersIds
        takeDistributedDeposit' tournamentId (playerId : backersIds)


-- Finish tournament

finishTournament :: TournamentId -> [(PlayerId, Balance)] -> IO ()
finishTournament tournamentId results = runDB $ do
    for_ results $ \(playerId, prize) -> do
        Participation _ _ backersIds <- getParticipation playerId
        updatePlayersBalanciesPortional' prize (playerId : backersIds)
    DB.deleteCascade tournamentId
  where
    getParticipation playerId = do
        Just (DB.Entity _ participation) <- DB.selectFirst
            [ ParticipationPlayerId ==. playerId
            , ParticipationTournamentId ==. tournamentId
            ] []
        return participation

handleFinishTournament :: Scotty.ActionM ()
handleFinishTournament = do
    Result tournamentId results <- Scotty.jsonData :: Scotty.ActionM Result
    liftIO $ finishTournament tournamentId (map unWinner results)
  where
    unWinner :: Winner -> (PlayerId, Balance)
    unWinner (Winner playerId prize) = (playerId, prize)


-- Router

route :: IO ()
route = Scotty.scotty 3000 $ do
    Scotty.get "/take" handleTakePointsFromPlayer
    Scotty.get "/fund" handleFundPointsToPlayer
    Scotty.get "/announceTournament" handleAnnounceTournament
    Scotty.get "/joinTournament" handleJoinTournament
    Scotty.post "/resultTournament" handleFinishTournament
    Scotty.get "/balance" handleGetBalance
    Scotty.get "/reset" handleReset

serve :: IO ()
serve = migrate >> route
