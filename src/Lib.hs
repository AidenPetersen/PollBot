{-# LANGUAGE OverloadedStrings #-}

module Lib (runBot) where

import Control.Monad (when)
import Data.Either
import Data.Emoji
import Data.Maybe (catMaybes)
import Data.Text (Text, isPrefixOf, pack, toLower, unpack)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.IO

runBot :: IO ()
runBot = do
  tokenFile <- openFile "TOKEN.token" ReadMode
  token <- hGetContents tokenFile
  putStrLn token
  userFacingError <-
    runDiscord $
      def
        { discordToken = pack token,
          discordOnEvent = eventHandler
        }
  TIO.putStrLn userFacingError

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (isPoll (messageText m)) $ do
    r <- restCall $ makeEmbed m
    let r' = head $ rights [r]
    mapM_ (addEmoji r') (take numEmojis emojis)
    where
      numEmojis = length $ catMaybes $ getOptions $ messageText m
      addEmoji :: Message -> String -> DiscordHandler ()
      addEmoji m s =
        do
          _ <- restCall $ R.CreateReaction (messageChannel m, messageId m) (pack s)
          pure ()

isPoll :: Text -> Bool
isPoll = ("+poll" `isPrefixOf`) . toLower

emojis :: [String]
emojis =
  [ "🇦",
    "🇧",
    "🇨",
    "🇩",
    "🇪",
    "🇫",
    "🇬",
    "🇭",
    "🇮",
    "🇯",
    "🇰",
    "🇱",
    "🇲",
    "🇳",
    "🇴",
    "🇵",
    "🇶",
    "🇷",
    "🇸",
    "🇹",
    "🇺",
    "🇻",
    "🇼",
    "🇽",
    "🇾",
    "🇿"
  ]

makeEmbed m =
  R.CreateMessageEmbed (messageChannel m) "" $
    def
      { createEmbedTitle = title,
        createEmbedDescription = options
      }
  where
    title = embedTitleText $ getTitle $ messageText m
    options = embedOpText $ getOptions $ messageText m

embedTitleText :: Maybe String -> Text
embedTitleText (Just s) = pack s
embedTitleText Nothing = pack "No title"

embedOpText :: [Maybe String] -> Text
embedOpText options = o
  where
    o = pack $ concat $ zipWith (\a b -> a ++ " " ++ b ++ "\n\n") emojis (catMaybes options)



getTitle :: Text -> Maybe String
getTitle t = getTitleRec (unpack t) False []
  where
    getTitleRec :: String -> Bool -> String -> Maybe String
    getTitleRec [] _ _ = Nothing
    getTitleRec (x : xs) inCurly acc
      | x == '}' && inCurly = Just (reverse acc)
      | not inCurly && x == '{' = getTitleRec xs True acc
      | inCurly && not (null xs) = getTitleRec xs inCurly (x : acc)
      | not inCurly && xs /= "" = getTitleRec xs inCurly acc
      | otherwise = Nothing

getOptions :: Text -> [Maybe String]
getOptions t = getOptionsRec (unpack t) False [] []
  where
    getOptionsRec :: String -> Bool -> [Maybe String] -> String -> [Maybe String]
    getOptionsRec [] _ _ _ = [Nothing]
    getOptionsRec (x : xs) inBrackets accLis acc
      | x == ']' && inBrackets =
        if null xs
          then reverse $ Just (reverse acc) : accLis
          else getOptionsRec xs False (Just (reverse acc) : accLis) []
      | null xs =
        if inBrackets
          then reverse (Nothing : accLis)
          else reverse accLis
      | inBrackets = getOptionsRec xs inBrackets accLis (x : acc)
      | x == '[' = getOptionsRec xs True accLis acc
      | otherwise = getOptionsRec xs inBrackets accLis acc
