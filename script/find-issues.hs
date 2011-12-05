import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import VotingGame.IssueParser
import VotingGame.Types

main = do
  putStrLn "Connecting..."
  dbh <-  connectPostgreSQL "dbname=scheduling user=scheduling"
  putStrLn "Finding issues..."
  issues <- getIssues activeFilter
  putStrLn "Inserting issues..."
  withTransaction dbh $ \conn -> do
    run conn "CREATE TEMPORARY TABLE tmp_issue (title TEXT, body TEXT, link TEXT)" []
    insertIssue conn `mapM_` issues
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
    putStrLn "Done!"
  where insertIssue dbh issue = do
          run dbh (unlines [ "INSERT INTO tmp_issue (title, body, link)"
                           , "VALUES (?, ?, ?)"
                           ])
                  [ toSql $ issueTitle issue
                  , toSql $ issueBody issue
                  , toSql $ issueLink issue
                  ]
