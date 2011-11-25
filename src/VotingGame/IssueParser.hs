{-# LANGUAGE OverloadedStrings #-}
module VotingGame.IssueParser where

import qualified Data.ByteString.Lazy as BS
import Text.XML (parseLBS_, def)
import Text.XML.Cursor
import Network.HTTP.Enumerator (simpleHttp)

import VotingGame.Types (Issue(..))

activeFilter :: String
activeFilter = "http://tickets.musicbrainz.org/sr/jira.issueviews:searchrequest-rss/10111/SearchRequest-10111.xml?tempMax=1000"

getIssues :: String -> IO [Issue]
getIssues = (fmap parseIssues) . simpleHttp

parseIssues :: BS.ByteString -> [Issue]
parseIssues rawXml = let doc = parseLBS_ def rawXml
                         cursor = fromDocument doc
                     in (cursor $/ element "channel" &/ element "item") >>= parseItem
  where parseItem c = return $ Issue (head $ c $/ element "title" &// content)
                                     (head $ c $/ element "link" &// content)
                                     (head $ c $/ element "description" &// content)
