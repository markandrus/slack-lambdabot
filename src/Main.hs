{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Binary.UTF8.String (decodeString)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Control.Lens ((^.), Getter, to, use)
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text, dropWhile, pack, unpack, stripPrefix)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import GHC.Generics (Generic)
import HTMLEntities.Decoder (htmlEncodedText)
import Lambdabot.Main
import Modules (modulesInfo)
import Prelude hiding (dropWhile, filter)
import System.Environment (lookupEnv)
import System.IO.Silently (capture)
import Text.Parsec (anyChar, between, char, eof, many, noneOf, oneOf, optional,
  parse, skipMany, skipMany1, string, try)
import Text.Parsec.Text (Parser)
import Web.Slack (Event(Message), Slack, SlackBot, SlackConfig(..), getId,
  runBot, selfUserId, session, slackSelf)
import Web.Slack.Message (sendMessage)

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

-- | A 'Command' contains a Haskell expression our 'lambdabot' can evaluate.
data Command where
  Eval :: !Text -> Command
  Type :: !Text -> Command
  deriving (Eq, Generic, NFData, Ord, Read, Show)

-- | View the Haskell expression inside of a 'Command'.
expression :: Getter Command Text
expression = to $ \case
  Eval expression' -> expression'
  Type expression' -> expression'

-- TODO(mroberts): Remove me.
prefix :: Getter Command Text
prefix = to $ \case
  Eval _ -> "eval"
  Type _ -> "type"

-- | Parse a 'Command'.
parseCommand :: Parser Command
parseCommand = try parseEval <|> parseType where

  -- | Parse an 'Eval' 'Command'.
  parseEval :: Parser Command
  parseEval
    =  skipMany spaceOrNewline
    *> optional parsePrefix
    *> (try (string ">")
   <|>  try (string "eval")
   <|>       string "run")
    *> skipMany1 spaceOrNewline
    *> (Eval . pack <$> parseCode)
   <*  skipMany spaceOrNewline
   <*  eof

  -- | Parse a 'Type' 'Command'.
  parseType :: Parser Command
  parseType
    =  skipMany spaceOrNewline
    *> optional parsePrefix
    *> (try (string "type")
   <|>       string "t")
    *> skipMany1 spaceOrNewline
    *> (Type . pack <$> parseCode)
   <*  skipMany spaceOrNewline
   <*  eof

  parsePrefix :: Parser Char
  parsePrefix
    =  try (char ':')
   <|> try (char '?')
   <|>      char '@'

  spaceOrNewline :: Parser Char
  spaceOrNewline = oneOf " \n"

  -- | Attempt to parse a code block surrounded in @```@. If that does not
  -- work, attempt to parse inline code surrounded in @`@. Finally, if that
  -- does not work, just treat the whole string as code.
  parseCode :: Parser String
  parseCode = try parseCodeBlock <|> try parseInlineCode <|> anyString where

    -- | Parses a code block surrounded in @```@.
    parseCodeBlock :: Parser String
    parseCodeBlock = between (string "```\n") (string "```") (many $ noneOf "`")

    -- | Parses inline code surrounded in @`@.
    parseInlineCode :: Parser String
    parseInlineCode = between (char '`') (char '`') (many $ noneOf "`")

    -- | Parse any string.
    anyString :: Parser String
    anyString = many anyChar

-------------------------------------------------------------------------------
-- Lambdabot
-------------------------------------------------------------------------------

-- | Run one or more commands against Lambdabot and capture the response.
lambdabot :: Command -> IO String
lambdabot command = do
  let request = void $ lambdabotMain modulesInfo
        [onStartupCmds :=> [unpack $ (command ^. prefix) <> " " <> (command ^. expression)]]
  (response, _) <- capture request
  return response

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
  -- let shouldReportParseError = isJust messageForMe
  let message = fromMaybe someMessage messageForMe
  let parsedCommand = parse parseCommand "" $ decodeHtml message
  case parsedCommand of
    Left error' ->
      -- when shouldReportParseError . sendMessage cid . pack $ show error'
      sendMessage cid . pack $ show error'
    Right command -> do
      rawResponse <- liftIO (pack . decodeString <$> lambdabot command)
      let response = "```\n" <> rawResponse <> "```"
      sendMessage cid response
slackBot _ = return ()

decodeHtml :: Text -> Text
decodeHtml = toStrict . toLazyText . htmlEncodedText

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  slackConfig <- envMkSlackConfig "SLACK_API_TOKEN"
  runBot slackConfig slackBot ()
