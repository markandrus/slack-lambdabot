{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Lens (use)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, dropWhile, filter, pack, unpack, stripPrefix)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import HTMLEntities.Decoder (htmlEncodedText)
-- import qualified HTMLEntities.Builder as HTMLEncoded
import Lambdabot.Main
import Modules (modulesInfo)
import Prelude hiding (dropWhile, filter)
import System.Environment (lookupEnv)
import System.IO.Silently (capture)
import Web.Slack (Event(Message), Slack, SlackBot, SlackConfig(..), getId,
  runBot, selfUserId, session, slackSelf)
import Web.Slack.Message (sendMessage)

-------------------------------------------------------------------------------
-- Lambdabot
-------------------------------------------------------------------------------

-- | Run one or more commands against Lambdabot and capture the response.
lambdabot :: Text -> IO String
lambdabot command = do
  let request = void $ lambdabotMain modulesInfo
        [onStartupCmds :=> [unpack command]]
  (response, _) <- capture request
  return response

-- Commented out below is how I might rewrite the lambdabot function. Lambdabot
-- itself is kind of kludgy when it comes to package management, etc. Maybe we
-- should just use mueval?
{-
lambdabot :: Text -> IO (Maybe String)
lambdabot (stripPrefix "t "     -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix ":t "    -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix "?t "    -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix "@t "    -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix "type "  -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix ":type " -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix "?type " -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix "@type " -> Just expr) = Just <$> lambdabotType expr
lambdabot (stripPrefix "> "     -> Just expr) = Just <$> lambdabotEval expr
lambdabot (stripPrefix "run "   -> Just expr) = Just <$> lambdabotEval expr
lambdabot (stripPrefix "?run "  -> Just expr) = Just <$> lambdabotEval expr
lambdabot (stripPrefix "@run "  -> Just expr) = Just <$> lambdabotEval expr
lambdabot expr = Just <$> lambdabotEval expr
lambdabot _ = return Nothing

lambdabotType :: Text -> IO (Maybe String)
lambdabotType = id
-}

-------------------------------------------------------------------------------
-- Slack
-------------------------------------------------------------------------------

-- | Construct a @SlackConfig@, taking the Slack API token from an environment
-- variable.
envMkSlackConfig :: String -> IO SlackConfig
envMkSlackConfig key
  =  mkSlackConfig
 <$> fromMaybe (error $ key <> " not set")
 <$> lookupEnv key

-- | Construct a @SlackConfig@ from a Slack API token.
mkSlackConfig :: String -> SlackConfig
mkSlackConfig apiToken = SlackConfig { _slackApiToken = apiToken }

-- | Get a message if it is for \"me\".
getMessageForMe :: Text -> Slack a (Maybe Text)
getMessageForMe message = do
  myId <- use $ session . slackSelf . selfUserId . getId
  let atMyId = "<@" <> myId <> ">"
  return $  dropWhile (\c -> c == ':' || c == ' ')
        <$> stripPrefix atMyId message

-- | Construct a @SlackBot@ from a name. This bot will pass messages addressed
-- to it to 'lambdabot' and relay 'lambdabot''s response.
slackBot :: SlackBot a
slackBot (Message cid _ someMessage _ _ _) = do
  messageForMe <- getMessageForMe someMessage
  case messageForMe of
    Nothing -> return ()
    Just message -> do
      let request = filter (/= '`') $ decodeHtml message
      rawResponse <- liftIO (pack . decodeString <$> lambdabot request)
      let response = "```\n" <> rawResponse <> "```"
      sendMessage cid response
slackBot _ = return ()

-- encodeHtml :: Text -> Text
-- encodeHtml = toStrict . toLazyText . HTMLEncoded.text

decodeHtml :: Text -> Text
decodeHtml = toStrict . toLazyText . htmlEncodedText

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  slackConfig <- envMkSlackConfig "SLACK_API_TOKEN"
  runBot slackConfig slackBot ()
