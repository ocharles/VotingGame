import Control.Monad (void)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Network.HTTP.Conduit
import VotingGame.IssueParser
import VotingGame.Types

main = do
  putStrLn "Connecting..."
  dbh <-  connectPostgreSQL "dbname=scheduling user=scheduling"
  request <- parseUrl "http://tickets.musicbrainz.org/sr/jira.issueviews:searchrequest-rss/10111/SearchRequest-10111.xml?tempMax=1000"
  putStrLn "Syncing..."
  withTransaction dbh $ \conn -> do
    run conn "CREATE TEMPORARY TABLE tmp_issue (title TEXT, body TEXT, link TEXT)" []
    withManager $ \m -> do
      res <- http request m
      responseBody res $= getIssues $$ CL.mapM_ (insertIssue conn)
    run conn (unlines [ "UPDATE issue SET visible = TRUE" ]) []
    run conn (unlines [ "UPDATE issue SET visible = FALSE"
                      , "FROM ("
                      , "SELECT DISTINCT link FROM issue"
                      , "EXCEPT"
                      , "SELECT DISTINCT link FROM tmp_issue"
                      , ") j WHERE j.link = issue.link"
                      ]) []
    run conn (unlines [ "INSERT INTO issue (title, body, link)"
                      , "SELECT title, body, link FROM tmp_issue"
                      , "WHERE link NOT IN (SELECT link FROM issue)"
                      ]) []
    run conn (unlines [ "UPDATE issue SET body = tmp.body FROM tmp_issue tmp"
                      , "WHERE tmp.link = issue.link" ]) []
    putStrLn "Done!"
  where insertIssue dbh issue = void $ do
          run dbh (unlines [ "INSERT INTO tmp_issue (title, body, link)"
                           , "VALUES (?, ?, ?)"
                           ])
                  [ toSql $ issueTitle issue
                  , toSql $ issueBody issue
                  , toSql $ issueLink issue
                  ]
