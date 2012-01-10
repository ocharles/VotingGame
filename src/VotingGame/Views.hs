{-# LANGUAGE OverloadedStrings #-}

module VotingGame.Views (landing, presentVote, results, nothingToDo) where

import Control.Monad (when)
import Data.Monoid (mempty)

import Data.Text (Text)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze (toHtml, toValue, (!), Html, preEscapedText)

import VotingGame.Types

nothingToDo :: Html
nothingToDo = pageTemplate $ do
  H.h1 "Nothing To Vote On!"
  H.p "You've voted on every issue we currently have to schedule... you rock!"
  H.p "Check back soon as we add add more issues to schedule."
  H.p "Thanks your help!"

landing :: Bool -> Html
landing loginFailed = pageTemplate $ do
    H.h1 "Welcome!"
    H.p "Welcome to the MusicBrainz scheduling game!"
    H.p $ toHtml $
      unlines [ "Due to the huge amount of tickets in our bug tracker, "
              , "we need community help to sort things out. In this game you "
              , "will be presented with random open tickets that need "
              , "scheduling. There are 3 possible options:"
              ]
    H.ol $ do
      H.li $ do
        H.strong "Within 3 months. "
        "This ticket must absolutely be fixed within 3 months from now."
      H.li $ do
        H.strong "Within 12 months. "
        toHtml $ unlines
          [ "This ticket should be fixed within a year from now. This "
          , "means the ticket should be fixed within the year, but"
          , "is not critical for the next 3 months"
          ]
      H.li $ do
        H.strong "Unscheduled. "
        toHtml $ unlines
          [ "This issue is not pressing and can be fixed much later." ]
    H.p $ toHtml $
      unlines [ "You also have the option of abstaining, meaning that "
              , "dont wish to influence scheduling of an issue at all."
              ]
    H.h1 "Let's Play!"
    H.p $ toHtml $
      unlines [ "Ready to play the scheduling game? Simply log in with your"
              , "MusicBrainz account and start voting!"
              ]
    H.form ! A.action "/login" ! A.method "POST" $ do
      when loginFailed $
        H.p "Oops! It seems those login details aren't correct, please try again"
      H.p ! A.class_ "row" $ do
        H.label "Editor name:"
        H.input ! A.name "editor"
      H.p ! A.class_ "row" $ do
        H.label "Password:"
        H.input ! A.name "password" ! A.type_ "password"
      H.p ! A.class_ "row indent" $ do
        H.input ! A.type_ "submit" ! A.value "Lets Play!"

presentVote :: Text -> Issue -> Html
presentVote editor issue = pageTemplate $ do
    H.h1 $ toHtml $ issueTitle issue
    voteForm
    H.div ! A.id "content" $
      preEscapedText $ issueBody issue
    H.script $ toHtml $ unlines [ "$('#content style').remove()" ]
    voteForm
  where voteForm =
          H.form ! A.action "/enter-vote" ! A.method "POST" $ do
            H.input ! A.type_ "hidden" ! A.name "editor" ! A.value (toValue editor)
            H.input ! A.type_ "hidden" ! A.name "issue" ! A.value (toValue $ issueLink issue)
            H.table ! A.id "vote-options" $ do
              H.tr $ do
                H.td ! A.colspan "2" $ H.input ! A.name "vote" ! A.type_ "submit" ! A.value "Within 3 Months"
                H.td ! A.colspan "2" $ H.input ! A.name "vote" ! A.type_ "submit" ! A.value "Within 12 Months"
                H.td ! A.colspan "2" $ H.input ! A.name "vote" ! A.type_ "submit" ! A.value "Unscheduled"
              H.tr $ do
                H.td ! A.colspan "3" $ H.input ! A.name "vote" ! A.type_ "submit" ! A.value "Abstain"
                H.td ! A.colspan "3" $ H.input ! A.name "vote" ! A.type_ "submit" ! A.value "Skip"


results :: [(Issue, Int, Int, Int)] -> Html
results rs = pageTemplate $ do
  H.h1 "And The Results Are In..."
  H.table $ do
    H.thead $
      H.tr $ do
        H.th "Issue"
        H.th "Within 3 Months"
        H.th "Within 12 Months"
        H.th "Unscheduled"
    H.tbody $ showResults rs
  where showResults rs' = showResult `mapM_` rs'
        showResult (issue, mo12, mo3, unsched) =
          H.tr $ do
            H.td $
	      H.a ! A.href (toValue $ issueLink issue) $ (toHtml $ issueTitle issue)
            H.td $ toHtml mo12
            H.td $ toHtml mo3
            H.td $ toHtml unsched

pageTemplate :: Html -> Html
pageTemplate body =
  H.html $ do
    H.head $ do
      H.link ! A.type_ "text/css" ! A.rel "stylesheet" ! A.href "/static/style.css"
      H.script ! A.src "http://ajax.googleapis.com/ajax/libs/jquery/1.6.1/jquery.min.js" $ mempty
      H.title "The Scheduling Game!"
    H.body $ body
