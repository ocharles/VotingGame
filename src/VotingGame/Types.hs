module VotingGame.Types (Issue(..), IssueVotes(..)) where

import Data.Text

data Issue = Issue { issueTitle :: Text
                   , issueLink :: Text
                   , issueBody :: Text
                   } deriving Show

data IssueVotes = IssueVotes Issue Int Int Int
