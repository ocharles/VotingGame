module VotingGame.Database (randomIssue, registerVote, findResults) where

import Database.HDBC (run, fromSql, toSql)
import Data.Map ((!))
import Data.Text (Text)
import Snap.Snaplet.Hdbc (withTransaction, query, HasHdbc)

import VotingGame.Types

randomIssue :: (Functor m, HasHdbc m c s) => Text -> m (Maybe Issue)
randomIssue editor = do
  row <- query "SELECT title, body, link FROM issue WHERE visible AND link NOT IN (SELECT issue FROM vote WHERE editor = ?) ORDER BY random() LIMIT 1" [ toSql editor ]
  case row of
    [] -> return Nothing
    rs -> return $ Just $ issueFromRow $ head rs

registerVote :: (Functor m, HasHdbc m c s) => Text -> Text -> Text -> m ()
registerVote editor vote issue = withTransaction $ \conn -> do
  run conn "INSERT INTO vote (vote, editor, issue) VALUES (?, ?, ?)" [ toSql vote, toSql editor, toSql issue ]
  return ()

findResults :: (Functor m, HasHdbc m c s) => m [(Issue, Int, Int, Int)]
findResults = do
  (map constructResult) `fmap` query "SELECT * FROM results" []
  where constructResult r = ( issueFromRow r
                            , fromSql $ r ! "mo3"
                            , fromSql $ r ! "mo12"
                            , fromSql $ r ! "unsched"
                            )

issueFromRow r = Issue { issueTitle = fromSql $ r ! "title"
                       , issueBody = fromSql $ r ! "body"
                       , issueLink = fromSql $ r ! "link"
                       }
