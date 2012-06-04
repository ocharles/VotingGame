{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Snap.Snaplet
import Snap.Http.Server.Config (emptyConfig)
import Snap.Snaplet.Session.Backends.CookieSession (initCookieSessionManager)
import Snap.Util.FileServe (serveDirectory)
import Snap.Snaplet.PostgresqlSimple

import VotingGame.Handlers
import VotingGame.Snaplet

votingGame :: SnapletInit VotingGame VotingGame
votingGame = makeSnaplet "VotingGame" "VotingGame" Nothing $ do
  sessionSnaplet <- nestSnaplet "session" session cookieSessionManager
  db <- nestSnaplet "db" dbLens pgsInit
  addRoutes [ ("/static", serveDirectory "resources")
            , ("/vote", presentVote)
            , ("/login", login)
            , ("/enter-vote", processVote)
            , ("/results", results)
            , ("", landing)
            ]
  return $ mkVotingGame sessionSnaplet db
  where cookieSessionManager =
          initCookieSessionManager "site_key.txt" "_bbsession" Nothing

main :: IO ()
main = serveSnaplet emptyConfig votingGame

