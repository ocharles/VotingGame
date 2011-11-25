module VotingGame.Database (randomIssue, registerVote) where

import Database.HDBC (run, fromSql, toSql)
import Data.Map ((!))
import Data.Text (Text)
import Snap.Snaplet.Hdbc (withTransaction, query, HasHdbc)

import VotingGame.Types

randomIssue :: (Functor m, HasHdbc m c s) => Text -> m Issue
randomIssue editor = do
  row <- head `fmap` query "SELECT title, body, link FROM issue WHERE link NOT IN (SELECT issue FROM vote WHERE editor = ?) ORDER BY random() LIMIT 1" [ toSql editor ]
  return Issue { issueTitle = fromSql $ row ! "title"
               , issueBody = fromSql $ row ! "body"
               , issueLink = fromSql $ row ! "link"
               }

registerVote :: (Functor m, HasHdbc m c s) => Text -> Text -> Text -> m ()
registerVote editor vote issue = withTransaction $ \conn -> do
  run conn "INSERT INTO vote (vote, editor, issue) VALUES (?, ?, ?)" [ toSql vote, toSql editor, toSql issue ]
  return ()

