{-# LANGUAGE OverloadedStrings #-}

module VotingGame.Handlers (landing, presentVote, login, processVote, results) where

import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Map ((!))
import Data.Maybe (fromJust, isJust)
import Network.Curl
import Snap.Core hiding (getRequest)
import Snap.Snaplet
import Snap.Snaplet.Session (getFromSession, setInSession, commitSession)
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
      blaze $ maybe Views.nothingToDo (Views.presentVote e) issue
    Nothing -> redirect "/"

login :: Handler VotingGame VotingGame ()
login = do
  editor <- fromJust `fmap` getParam "editor"
  password <- fromJust `fmap` getParam "password"
  response <- liftIO $ withCurlDo $
    curlGetResponse "http://musicbrainz.org/ws/2/collection"
      [ CurlUserPwd $ (BS.unpack editor) ++ ":" ++ (BS.unpack password)
      , CurlHttpAuth [HttpAuthDigest]
      ]
  case (respCurlCode response == CurlOK) of
    True -> do
      with session  $ do
        setInSession "editor" (decodeUtf8 editor)
        commitSession
      redirect "/vote"
    False -> redirect "/?loginFailed"

landing :: Handler VotingGame VotingGame ()
landing = do
  loginFailed <- getParam "loginFailed"
  blaze $ Views.landing (isJust loginFailed)

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
