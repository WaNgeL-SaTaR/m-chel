{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}

module Request.Backend.CouchDb where

import           Database.CouchDB.Conduit
import           Database.CouchDB.Conduit.DB
import           Database.CouchDB.Conduit.Design
import           Database.CouchDB.Conduit.Explicit
import           Database.CouchDB.Conduit.View
import           Database.CouchDB.Conduit.View.Query
import Database.CouchDB.Conduit.Attachment
--import Text.JSON.Generic

import           Request.Common

import           Control.Applicative
import           Control.Monad
import "mtl"          Control.Monad.Trans

import           Data.List

import           Control.Exception
import           Data.Aeson.Types
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Internal            as BI
import qualified Data.ByteString.Lazy                as BL
import qualified Data.ByteString.Lazy.Internal       as BLI
import qualified Data.Text.Lazy as T
import           Data.Conduit
import           Data.Conduit                        (ResumableSource, ($$+-))
import qualified Data.Conduit.List                   as CL
import           Data.Maybe
import           Data.String
import qualified Data.UUID
import           Prelude                             hiding (catch)
import           System.Random
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import           Config


import qualified Network.HTTP.Types                  as HT


import           Database.CouchDB.Conduit.LowLevel   (CouchResponse, couch,
                                                      couch', protect, protect')
import qualified Network.HTTP.Conduit                as H


--instance FromJSON Category

-- addArticle article =
--   do runCouchDB' $ newDoc (db "articles") (toJSON article)
--      return 5

-- toStrict1 :: BL.ByteString -> B.ByteString
-- toStrict1 = B.concat . BL.toChunks

mycon = def { couchLogin = myLogin,
              couchPass = myPass}


-- getArticle document =
--   do runCouchDB' $ getDoc (db "articles") (doc document)
genUUID :: IO String
genUUID = (show) `liftM`(randomIO ::IO Data.UUID.UUID)

add :: ToJSON a => String -> a -> IO (B.ByteString)
add base val = do uid <-  genUUID
                  add' base uid val
                  return (fromString uid)


add' :: ToJSON a => String -> String -> a -> IO (B.ByteString)
add' base id val =
  runCouch mycon $ do
    couchPut' (fromString base) (fromString id) [] $ val
--    liftIO $ print a
    return (fromString id)

getFeedbacks :: Bool -> IO [Feedback]
getFeedbacks isConfirmed = view myBase myDesign "feedbackByDate" [ QPStartKey [isConfirmed]
                                                                 , QPEndKey  (isConfirmed, qpUnit)]

allFeedbacks :: Bool -> IO [Feedback]
allFeedbacks isConfirmed = view myBase myDesign "feedbackByDate" []


-- FIXME: dont use head!!!!
tagsInCategory :: String -> IO [String]
tagsInCategory cat = (head . nub . fmap (nub)) `liftM` view myBase myDesign "tagsByCategory" [QPReduce True,
                                                                        QPGroup,
                                                                        QPKey cat]

itemsFromCategoryAndTag :: String -> String -> IO [Item]
itemsFromCategoryAndTag cat tag = view myBase myDesign "byCategoryAndTag" [QPKey [cat,tag]]

itemsFromCategory :: String -> IO [Item]
itemsFromCategory cat = view myBase myDesign "byCategory" [QPKey cat]

itemsFromCategoryByCost :: String -> Bool -> IO [Item]
itemsFromCategoryByCost cat isAsc =
  view myBase myDesign "byCostCategory" ([ QPStartKey [cat]
                                        , QPEndKey  (cat, qpUnit)
                                        ] ++ (if isAsc then [] else [QPDescending]))


--FIXME:: WTF!!!!!&!!#*&$#( ЯЯЯЯЯЯЯЯЯЯЯЯ
searchByString :: String -> IO [Item]
searchByString str =
  view myBase myDesign "searchSubstring" [QPStartKey  str,
                                          QPEndKey (str ++ "ЯЯЯЯЯЯЯЯЯЯЯЯЯЯЯЯЯЯЯЯ"),
                                          QPLimit 20]

deleteCategory :: String -> IO ()
deleteCategory cat = do
  cats <- getCategories
  let myCat = find ((==cat) . catName) cats
  case myCat of
    Just val -> void $ deleteItem (catId val)
    Nothing -> return ()

deleteCategoryWithContent :: String -> IO ()
deleteCategoryWithContent cat = do
  items <- itemsFromCategory cat
  mapM_ (deleteItem . itemId) items
  deleteCategory cat
  return ()



allFromRequests :: IO [Request]
allFromRequests  = view myBase myDesign "requests" [QPDescending]

newRequests :: IO [Request]
newRequests  = view myBase myDesign "newRequests" [QPStartKey  [True]
                                                  , QPEndKey   (True, qpUnit)
                                                  ]
oldRequests :: IO [Request]
oldRequests  = view myBase myDesign "newRequests" [QPStartKey  [False],
                                                   QPEndKey   (False, qpUnit),
                                                   QPDescending]

getCategories :: IO [Category]
getCategories = view myBase myDesign "categories" []

view :: (FromJSON a) =>  String -> String -> String -> [CouchQP] -> IO [a]
view db design v params =
  runCouch mycon $ do
    src <- couchView (fromString db) (fromString design) (fromString v)
           (mkQuery params)
--    src $$ CL.mapM_ (\x -> liftIO $ print x)
    res <- src $= rowValue $$ CL.consume
    let parsed = map (\x -> parseMaybe parseJSON x) res
    return $ catMaybes parsed

checkUserPass :: String -> String -> IO Bool
checkUserPass name pass | pass == webPass = return True
                        | otherwise      = return False
--  do s <- get "testapp" "admin"

--getAllCategories =

get :: (FromJSON a) => String -> String -> IO (Maybe a)
get base articleId =
  (runCouch mycon $ do (a,b) <- couchGet (fromString base) (fromString articleId) []
                       return (Just b))
  `catch` (\(e ::CouchError) -> return Nothing)

prepareDb :: IO ()
prepareDb =
  do runCouch mycon $ do couchPutDB_ (fromString myBase)
                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "categories"
                           "function(doc) {\n  if (doc.catName) {\n    emit(doc.catName, doc);\n  }\n}"
                           Nothing

                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "byCategory"
                           "function(doc) {\n  if (doc.category)\n  emit(doc.category, doc);\n}"
                           Nothing

                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "requests"
                           "function(doc) {\n  if (doc.requestDate) {\n    emit(doc.requestDate, doc);\n  }\n}"
                           Nothing
                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "newRequests"
                           "function(doc) {\n    if (doc.requestDate) {\n      emit([doc.requestNew,doc.requestDate], doc);\n    }\n}"
                           Nothing

                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "byCostCategory"
                           "function(doc) {\n  if (doc.category && doc.cost >= 0)\n  emit([doc.category,doc.cost], doc);\n}"
                           Nothing

                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "searchSubstring"
                           "function(doc) {\n    var i;\n    var title = doc.title;\n    \n    if (title) {\n        title = title.replace(/[!.,;\\]\\[]+/g,\"\").toLowerCase().split(\" \");\n        for (i = 0; i < title.length; i += 1) {\n            emit(title.slice(i).join(' '), doc);\n        }\n    }\n}"
                           Nothing

                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "byCategoryAndTag"
                           "function(doc) {\n  if (doc.category && doc.tags) {\n     doc.tags.forEach(function(tag) {\n       emit([doc.category, tag], doc);\n     })\n  }\n}"
                           Nothing

                         couchPutView (fromString myBase)
                           (fromString myDesign)
                           "tagsByCategory"
                           "function(doc) {\n  if (doc.category && doc.tags) {\n     doc.tags.forEach(function(tag) {\n       emit(doc.category, tag);\n     })\n  }\n}"
                           (Just "function(keys, values) {\n\n  var o = {}, i, l = values.length, r = [];\n\n  for (i = 0; i < l; i += 1) {\n      o[values[i]] = values[i];\n  }\n\n  for (i in o) {\n    r.push(o[i]);\n  }\n\n  return Array.prototype.concat.apply([], r);\n}\n")
                           -- (Just "function(keys, values) {\n\n  var o = {}, i, l = values.length, r = [];\n\n  for (i = 0; i < l; i += 1) {\n    o[values[i]] = values[i];\n  }\n\n  for (i in o) {\n    r.push(o[i]);\n  }\n\n  return Array.prototype.concat.apply([], r);\n}\n")

     return ()



couchPutAttach1 :: MonadCouch m =>
                  Path             -- ^ Database
                  -> Path           -- ^ Document
                  -> Path           -- ^ Attachment
                  -> Revision       -- ^ Revision
                  -> B.ByteString  -- ^ Content type
                  -> B.ByteString  -- ^ Content body
                  -> m ()
couchPutAttach1 db doc att rev contentType content = void $ couch HT.methodPut
                            (mkPath [db,doc,att])
                            [(HT.hContentType, contentType)]
                            (if (rev /= "") then [("rev", Just rev)] else [])
                            (H.RequestBodyBS content)
                            protect'


couchGetAttach1 :: MonadCouch m =>
                  Path             -- ^ Database
                  -> Path           -- ^ Document
                  -> Path           -- ^ Attachment
                  -> m (B.ByteString, B.ByteString)
couchGetAttach1 db doc att = do
  H.Response _ _ hs bsrc <- couch HT.methodGet
                            (mkPath [db,doc,att])
                            []
                            []
                            (H.RequestBodyBS B.empty)
                            protect'
  body <- bsrc $$+- CL.consume
  let contentType = peekContentType hs
  return (B.concat body, contentType)
    where
      peekVal a b = fromJust $ lookup a b
      peekContentType a = peekVal "Content-Type" a


getAttach :: Path -> Path -> Path -> IO (B.ByteString, B.ByteString)
getAttach a b c =  runCouch mycon $ do src <- couchGetAttach1 a b c
                                       return src

getImage id image = getAttach (fromString myBase) id image


--putAttach :: String -> String -> String -> IO ()
putAttach a b c d e =  do runCouch mycon $
                            do rev <- couchRev' a b
                               liftIO $ print $ "rev" ++ show rev
                               couchPutAttach1  a b c rev d e

delImage _id image = delAttach (fromString myBase) _id image

delAttach base _id path = do
  runCouch mycon $
    do rev <- couchRev' base _id
       couchDeleteAttach base _id path rev

putStandAttach a b c d e =  do runCouch mycon $ couchPutAttach1 a b c "" d e

deleteDoc :: String -> String -> IO ()
deleteDoc base articleId =
  runCouch mycon $ do rev <- couchRev' (fromString base) (fromString articleId)

                      when (rev /= "")
                        $ couchDelete (fromString base) (fromString articleId) rev
                      return ()

deleteItem :: String -> IO ()
deleteItem _id = do deleteDoc myBase _id


putImage :: Path -> Path -> BI.ByteString -> BI.ByteString -> IO ()
putImage doc name contentType content = putAttach (fromString myBase) doc (name) contentType content

addRequest :: Request -> IO (B.ByteString)
addRequest request = add myBase request

getRequest :: String -> IO (Maybe Request)
getRequest _id = get myBase _id

addItem :: Item -> IO (B.ByteString)
addItem = add myBase

getItem :: String -> IO (Maybe Item)
getItem _id = get myBase _id

addCategory :: Category -> IO (B.ByteString)
addCategory = add myBase


addContent :: Content -> IO (B.ByteString)
addContent = add myBase


getContent :: String -> IO (Maybe Content)
getContent _id = get myBase _id

getCategory :: String -> IO (Maybe Category)
getCategory _id = get myBase _id

changeItem :: String -> Item -> IO (B.ByteString)
changeItem _id val = add' myBase _id val

changeRequest :: String -> Request -> IO (B.ByteString)
changeRequest _id val = add' myBase _id val

changeCategory :: String -> Category -> IO (B.ByteString)
changeCategory _id val = add' myBase _id val

changeContent :: String -> Content -> IO (B.ByteString)
changeContent _id val = add' myBase _id val
