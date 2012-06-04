{-# LANGUAGE OverloadedStrings #-}

module VotingGame.Database (randomIssue, registerVote, findResults) where

import Control.Applicative
import Data.Map ((!))
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import Snap.Snaplet.PostgresqlSimple

import VotingGame.Types

instance FromRow Issue where
  fromRow = Issue <$> field <*> field <*> field

instance FromRow IssueVotes where
  fromRow = IssueVotes <$> fromRow <*> field <*> field <*> field

randomIssue :: (Functor m, HasPostgres m) => Text -> m (Maybe Issue)
randomIssue editor = do
  row <- query "SELECT title, link, body FROM issue WHERE visible AND link NOT IN (SELECT issue FROM vote WHERE editor = ?) ORDER BY random() LIMIT 1" $ Only editor
  case row of
    [] -> return Nothing
    rs -> return $ Just $ head rs

registerVote :: (Functor m, HasPostgres m) => Text -> Text -> Text -> m ()
registerVote editor vote issue = withTransaction $ do
  execute "INSERT INTO vote (vote, editor, issue) VALUES (?, ?, ?)" (vote, editor, issue)
  return ()

findResults :: (Functor m, HasPostgres m) => m [IssueVotes]
findResults = query_ "SELECT title, link, body, mo3, mo12, unsched FROM results"
