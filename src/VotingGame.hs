{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Snap.Snaplet
import Snap.Http.Server.Config (emptyConfig)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Snap.Util.FileServe (serveDirectory)
import Snap.Snaplet.Hdbc

import VotingGame.Handlers (login, presentVote, landing, processVote)
import VotingGame.Snaplet

votingGame :: SnapletInit VotingGame VotingGame
votingGame = makeSnaplet "VotingGame" "VotingGame" Nothing $ do
  sessionSnaplet <- nestSnaplet "session" session cookieSessionManager
  let pg = connectPostgreSQL "dbname=scheduling user=scheduling"
  db <- nestSnaplet "hdbc" dbLens $ hdbcInit pg
  addRoutes [ ("/static", serveDirectory "resources")
            , ("/vote", presentVote)
            , ("/login", login)
            , ("/enter-vote", processVote)
            , ("", landing)
            ]
  return $ mkVotingGame sessionSnaplet db
  where cookieSessionManager =
          initCookieSessionManager "site_key.txt" "_bbsession" Nothing

main :: IO ()
main = serveSnaplet emptyConfig votingGame

