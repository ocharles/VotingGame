{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module VotingGame.Snaplet (VotingGame, mkVotingGame, session, dbLens) where

import Control.Monad.State.Class (get)
import Data.Lens.Template (makeLenses)
import Snap.Snaplet
import Snap.Snaplet.Session (SessionManager)
import Snap.Snaplet.PostgresqlSimple

data VotingGame = VotingGame { _session :: Snaplet SessionManager
                             , _dbLens :: Snaplet Postgres
                             }

makeLenses [''VotingGame]

mkVotingGame :: Snaplet SessionManager -> Snaplet Postgres -> VotingGame
mkVotingGame = VotingGame

instance HasPostgres (Handler VotingGame VotingGame) where
  getPostgresState = with dbLens $ get
