module VotingGame.Types (Issue(..)) where

import Data.Text

data Issue = Issue { issueTitle :: Text
                   , issueLink :: Text
                   , issueBody :: Text
                   }
