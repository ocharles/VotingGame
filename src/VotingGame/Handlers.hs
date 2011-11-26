{-# LANGUAGE OverloadedStrings #-}

module VotingGame.Handlers (landing, presentVote, login, processVote, results) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Map ((!))
import Data.Maybe (fromJust)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Session
import Snap.Blaze (blaze)

import qualified VotingGame.Views as Views
import VotingGame.Snaplet
import VotingGame.Database

presentVote :: Handler VotingGame VotingGame ()
presentVote = do
  editor <- with session $ getFromSession "editor"
  case editor of
    Just e -> do
      issue <- randomIssue e
      blaze $ Views.presentVote e issue
    Nothing -> redirect "/"
  

login :: Handler VotingGame VotingGame ()
login = do
  editor <- fromJust `fmap` getParam "editor"
  with session $ do
    setInSession "editor" (decodeUtf8 editor)
    commitSession
  redirect "/vote"

landing :: Handler VotingGame VotingGame ()
landing = blaze $ Views.landing

processVote :: Handler VotingGame VotingGame ()
processVote = do
  params <- getParams
  let p name = decodeUtf8 $ head $ params ! name
  case voteMap (p "vote") of
    Just vote -> registerVote (p "editor") vote (p "issue")
    Nothing -> return ()
  redirect "/vote"

voteMap :: Text -> Maybe Text
voteMap v = lookup v [("Within 3 Months", "3months")
                     ,("Within 12 Months", "12months")
                     ,("Unscheduled", "unscheduled")
                     ,("Abstain", "abstain")
                     ]

results :: Handler VotingGame VotingGame ()
results = do
  rs <- findResults
  blaze $ Views.results rs
