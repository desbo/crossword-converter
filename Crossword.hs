{-# LANGUAGE OverloadedStrings #-}

module Crossword (Crossword, parseCrossword) where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Time

data CrosswordType = Quick | Cryptic 
  deriving Show

data Coordinate = Coordinate Int Int 
  deriving Show

data Dimensions = Dimensions Int Int
  deriving Show

data Entry = Entry {
    number :: Int
  , direction :: String
  , position :: Coordinate
  , length :: Int
  , clue :: String
  , solution :: String
} deriving Show

data Crossword = Crossword {
    crosswordType :: CrosswordType
  , id :: Int
  , name :: String
  , pdfUrl :: String
  , date :: Day
  , dimensions :: Dimensions
  , entries :: [Entry]
} deriving Show

toType :: String -> CrosswordType
toType "quick"   = Quick
toType "cryptic" = Cryptic

toDay :: String -> Day
toDay s = readTime defaultTimeLocale "%Y-%m-%d" s

instance FromJSON Entry where
  parseJSON (Object o) = Entry <$>
                         o .: "number" <*>
                         o .: "direction" <*>
                         o .: "position" <*>
                         o .: "length" <*>
                         o .: "clue" <*>
                         o .: "solution"

instance FromJSON Dimensions where
  parseJSON (Object o) = Dimensions <$> o .: "cols" <*> o .: "rows"

instance FromJSON Coordinate where
  parseJSON (Object o) = Coordinate <$> o .: "x" <*> o .: "y"

instance FromJSON Crossword where
  parseJSON (Object o) = Crossword <$>
                         liftM toType (o .: "type") <*>
                         o .: "number" <*>
                         o .: "name" <*>
                         o .: "pdf" <*>
                         liftM toDay (o .: "date") <*>
                         o .: "dimensions" <*>
                         o .: "entries"

parseCrossword json = decode json :: Maybe Crossword
