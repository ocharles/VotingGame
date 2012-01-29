{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell #-}
module VotingGame.Snaplet (VotingGame, mkVotingGame, session, dbLens) where

import Control.Monad.State.Class (get)
import Database.HDBC.PostgreSQL (Connection)
import Data.Lens.Template (makeLenses)
import Snap.Snaplet
import Snap.Snaplet.Session (SessionManager)
import Snap.Snaplet.Hdbc

data VotingGame = VotingGame { _session :: Snaplet SessionManager
                             , _dbLens :: Snaplet (HdbcSnaplet Connection IO)
                             }

makeLenses [''VotingGame]

mkVotingGame :: Snaplet SessionManager -> Snaplet (HdbcSnaplet Connection IO) -> VotingGame
mkVotingGame = VotingGame

instance HasHdbc (Handler VotingGame VotingGame) Connection IO where
  getHdbcState = with dbLens $ get
