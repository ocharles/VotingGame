{-# LANGUAGE OverloadedStrings #-}
module VotingGame.IssueParser (getIssues, activeFilter) where

import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.XML.Types
import Text.XML.Stream.Parse (parseBytes, def)
import Text.XML.Cursor
import Text.XML (fromXMLNode)
import Text.XML.Unresolved

import VotingGame.Types (Issue(..))

activeFilter :: String
activeFilter = "http://tickets.musicbrainz.org/sr/jira.issueviews:searchrequest-rss/10111/SearchRequest-10111.xml?tempMax=1000"

getIssues :: ResourceThrow m => Conduit ByteString m Issue
getIssues = parseBytes def =$= sequenceSink () preludeSink
  where preludeSink _ = goTo "item" >> return (StartConduit itemConduit)
        itemConduit = sequenceSink () itemSink
        goTo n = do
          head <- CL.peek
          case head of
            Just (EventBeginElement n' _) | n' == n -> return ()
            Nothing -> return ()
            _ -> CL.drop 1 >> goTo n
        parseItem c = Issue (head $ c $/ element "title" &// content)
                            (head $ c $/ element "link" &// content)
                            (safeHead "" $ c $/ element "description" &// content)
        safeHead _ (a:b) = a
        safeHead h _ = h
        itemSink _ = do
          element <- fromXMLNode . NodeElement <$> fromEv <* goTo "item"
          case element of
            Left e -> error "I have no idea what I'm doing"
            Right n -> return $ Emit () . (:[]) $ parseItem $ fromNode n

fromEv :: ResourceThrow m => Sink Event m Element
fromEv = require goE
  where
    many f =
        go id
      where
        go front = do
            x <- f
            case x of
                Nothing -> return $ front []
                Just y -> go (front . (:) y)
    dropReturn x = CL.drop 1 >> return x
    require f = do
        x <- f
        case x of
            Just y -> return y
            Nothing -> do
                y <- CL.head
                lift $ resourceThrow $ InvalidEventStream $ "Document must have a single root element, got: " ++ show y
    goE = do
        x <- CL.peek
        case x of
            Just (EventBeginElement n as) -> Just <$> goE' n as
            _ -> return Nothing
    goE' n as = do
        CL.drop 1
        ns <- many goN
        y <- CL.head
        if y == Just (EventEndElement n)
            then return $ Element n as $ compressNodes ns
            else lift $ resourceThrow $ InvalidEventStream $ "Missing end element for " ++ show n ++ ", got: " ++ show y
    goN = do
        x <- CL.peek
        case x of
            Just (EventBeginElement n as) -> (Just . NodeElement) <$> goE' n as
            Just (EventInstruction i) -> dropReturn $ Just $ NodeInstruction i
            Just (EventContent c) -> dropReturn $ Just $ NodeContent c
            Just (EventComment t) -> dropReturn $ Just $ NodeComment t
            Just (EventCDATA t) -> dropReturn $ Just $ NodeContent $ ContentText t
            _ -> return Nothing

compressNodes :: [Node] -> [Node]
compressNodes [] = []
compressNodes [x] = [x]
compressNodes (NodeContent (ContentText x) : NodeContent (ContentText y) : z) =
    compressNodes $ NodeContent (ContentText $ x `T.append` y) : z
compressNodes (x:xs) = x : compressNodes xs
