{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Request.Common where

import Data.Data
import GHC.Generics
import Data.Aeson.Types
import Control.Applicative

import Data.List.Split
import Data.Time
import Data.List
import qualified Data.Text as T



parseTags :: String -> [String]
parseTags = nub . filter (/= "") . map (T.unpack . T.strip . T.pack) . splitOn ","
--parseTags = splitOn ","

--Just fromList [("evil_plotting_raccoon.jpg",Object fromList [("stub",Bool True),("revpos",Number 32),("length",Number 41915),("content_type",String "image/jpeg"),("digest",String "md5-CKtT5WWRLkmGDD3/DhK6FQ==")])]

data RequestItem = RequestItem { requestItem :: String
                               , requestItemName :: String
                               , requestCost :: String
                               , requestCount :: String}
               deriving (Show, Ord, Eq, Generic)

instance FromJSON RequestItem where
  parseJSON (Object v) = RequestItem <$>
                  v .: "requestItem" <*>
                  v .:? "requestItemName" .!= "" <*>
                  v .:? "requestCost" .!= "" <*>
                  v .: "requestCount"

instance ToJSON RequestItem

data Request = Request { requestId :: String
                       , requestName :: String
                       , requestDate        :: UTCTime
                       , requestDescription :: String
                       , requestNew :: Bool
                       , requestTelephone   :: String
                       , requestItems :: [RequestItem]}
               deriving (Show, Ord, Eq, Generic)

instance ToJSON Request

instance FromJSON Request where
  parseJSON (Object v) = Request <$>
                  v .: "_id"  <*>
                  v .:? "requestName" .!= ""  <*>
                  v .: "requestDate"   <*>
                  v .:? "requestDescription" .!= ""  <*>
                  v .:  "requestNew"  <*>
                  v .:  "requestTelephone"  <*>
                  v .:  "requestItems"


data Item = Item { itemId      :: String
                 , title       :: String
                 , date        :: String
                 , description :: String
                 , category    :: String
                 , cost        :: Integer
                 , old_cost    :: Integer
                 , tags        :: [String]
                 , itemAttach :: Maybe Object
                 }
               deriving (Show, Eq, Generic)


instance ToJSON Item where
  toJSON (x) = object $ [ "_id" .= itemId x
                        , "title" .= title x
                        , "date" .= date x
                        , "description" .= description x
                        , "category" .= category x
                        , "cost" .= cost x
                        , "old_cost" .= old_cost x
                        , "tags" .= tags x
                        ] ++ (if (Nothing == itemAttach x)
                              then []
                              else ["_attachments" .= itemAttach x])

instance FromJSON Item where
  parseJSON (Object v) = Item <$>
                  v .: "_id" <*>
                  v .: "title" .!= "" <*>
                  v .:? "date" .!= ""  <*>
                  v .:? "description" .!= "" <*>
                  v .:? "category" .!= "" <*>
                  v .:? "cost" .!= 0 <*>
                  v .:? "old_cost" .!= 0 <*>
                  v .:? "tags" .!= [] <*>
                  v .:? "_attachments" .!= Nothing


data Category = Category { catId :: String
                         , catTitle :: String
                         , catContent :: String
                         , catName  :: String}
               deriving (Show, Ord, Eq, Generic)

instance ToJSON Category where
   toJSON (x) = object [ "catId" .= catId x
                       , "catTitle" .= catTitle x
                       , "catContent" .= catContent x
                       , "catName" .= catName x]

instance FromJSON Category where
  parseJSON (Object v) = Category <$>
                         v .: "_id" <*>
                         v .: "catTitle" <*>
                         v .:? "catContent" .!= "" <*>
                         v .: "catName"

data Content = Content { contentId :: String
                       , contentTitle :: String
                       , contentBody  :: String}
               deriving (Show, Ord, Eq, Generic)

instance ToJSON Content

instance FromJSON Content where
  parseJSON (Object v) = Content <$>
                         v .: "_id" <*>
                         v .: "contentTitle"  <*>
                         v .: "contentBody"




--data Category = Category { categoryId   :: String}
--               deriving (Show, Data, Typeable, Ord, Eq, Generic)


data Feedback = Feedback { feedbackId :: String
                         , feedbackContent :: String
                         , feedbackName  :: String
                         , feedbackContact :: String
                         , feedbackDate :: String
                         , feedbackConfirm :: Bool}
               deriving (Show, Ord, Eq, Generic)

instance ToJSON Feedback

instance FromJSON Feedback where
  parseJSON (Object v) = Feedback <$>
                         v .: "_id" <*>
                         v .: "feedbackContent" <*>
                         v .: "feedbackName" <*>
                         v .: "feedbackContact" <*>
                         v .: "feedbackDate" <*>
                         v .: "feedbackConfirm"