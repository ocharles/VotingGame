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
  insertIssue dbh `mapM_` issues
  putStrLn "Done!"
  commit dbh
  where insertIssue dbh issue = do
          run dbh "INSERT INTO issue (title, body, link) VALUES (?, ?, ?)"
                  [ toSql $ issueTitle issue
                  , toSql $ issueBody issue
                  , toSql $ issueLink issue
                  ]
