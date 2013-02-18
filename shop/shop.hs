{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
module Main where

import Happstack.Server hiding (Request)
import Data.Aeson
import Data.Aeson.Types
--import Text.JSON.Generic
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.UTF8 as UTF8S
import Control.Monad
import "mtl" Control.Monad.Trans
import Data.Maybe
import Data.List
import Data.Char
import Data.List.Split
import Request.Common
import Request.Backend.CouchDb
import Data.Aeson.Types
import Data.String
import Data.Time
import Control.Applicative
import Text.Pandoc
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Config

import System.Process(system)
import System.FilePath
--testProjectJSON = encodeJSON
--instance Data Article
--instance Typeable Article
encodeJSON :: ToJSON a => a -> UTF8.ByteString
encodeJSON val = encode val

jsonResponse func = do
  cat <- optional $ look "category"
  setHeaderM "Content-type" "application/json; charset=UTF-8"
  setHeaderM "Cache-Control" "no-store, no-cache, must-revalidate"
  setHeaderM "Pragma" "no-cache"
  liftIO $ print ("GET: json [" ++ show cat ++ "]")
  ok $ toResponseBS (C.pack "application/json") ( func)

markThis2 = unlines . map markThis2 . lines

markThis a = writeHtmlString defaultWriterOptions
              (readMarkdown defaultParserState (filter (/= '\r') a))

markContent content =
  content { contentBody = markThis oldBody}
  where oldBody = contentBody content

markCategory content =
  content { catContent = markThis oldBody}
  where oldBody = catContent content


uploadAndConvert _id filepath = do
  let resultName = takeBaseName filepath ++ ".jpeg"
  let outFilename = dropExtension filepath ++ ".jpeg"
  system $ concat ["convert -quality 75 -resize '>1024x1024' '",filepath, "' '", outFilename,"'"]
  content <- B.readFile outFilename
  putImage _id (UTF8S.fromString resultName)
    (fromString $ concat ["image", "/", "jpeg"]) content
  system $ concat ["rm -f '", outFilename, "'"]
  return ()


jsonFromCategory :: Bool -> ServerPart Response
jsonFromCategory isAsc =
  do methodM GET
     cat <- look "category"
     res <- liftIO $ itemsFromCategoryByCost cat isAsc
     jsonResponse $ encodeJSON res


--convertAttach xs = map (\x -> fmap (fmap head) (itemAttach x)) xs

jsonCategoryTags :: ServerPart Response
jsonCategoryTags =
  do methodM GET
     cat <- look "category"
     res <- liftIO $ tagsInCategory cat
     jsonResponse $ encodeJSON $ res

jsonFromCategoryTag :: ServerPart Response
jsonFromCategoryTag =
  do methodM GET
     cat <- look "category"
     tag <- look "tag"
     res <- liftIO $ itemsFromCategoryAndTag cat tag
     jsonResponse $ encodeJSON res

jsonSearchItem :: ServerPart Response
jsonSearchItem =
  do methodM GET
     str <- look "str"
     res <- liftIO $ searchByString str
     jsonResponse $ encodeJSON res


jsonItem :: ServerPart Response
jsonItem =
  do methodM GET
     _id <- look "id"
     res <- liftIO $ getItem _id
     jsonResponse $ encodeJSON res


jsonContent :: ServerPart Response
jsonContent =
  do methodM GET
     _id <- look "id"
     res <- liftIO $ getContent _id
     jsonResponse $ encodeJSON $ fmap (markContent) res

jsonRawContent :: ServerPart Response
jsonRawContent =
  do methodM GET
     _id <- look "id"
     res <- liftIO $ getContent _id
     jsonResponse $ encodeJSON $ res


jsonEditContent :: ServerPart Response
jsonEditContent =
  do checkAuth
     methodM POST
     _id <- look "id"
     title <- look "contentTitle"
     body <- look "contentBody"
     let date = ""
     liftIO $ print "CONTENT-CHANGE"
     liftIO $ changeContent _id
       (Content { contentId = ""
                , contentTitle = title
                , contentBody = body
                })
     jsonResponse $ encodeJSON ("ok" ::String)

jsonEdit :: ServerPart Response
jsonEdit =
  do checkAuth
     methodM POST
     _id <- look "id"
     title <- look "title"
     description <- look "description"
     category <- look "category"
     cost <- look "cost"
     old_cost <- look "old_cost"
     tags <- look "tags"
     let date = ""

     Just oldItem <- liftIO $ getItem _id
     liftIO $ changeItem _id
       (Item "" title
        date
        description
        category (readNumOr cost 0) (readNumOr old_cost 0) (parseTags tags)
       (itemAttach oldItem))
     jsonResponse $ encodeJSON ("ok" ::String)


jsonEditCategory :: ServerPart Response
jsonEditCategory =
  do checkAuth
     methodM POST
     liftIO $ print "EDIT CATEGORY"
     _id <- look "catId"
     title <- look "catTitle"
     name <- look "catName"
     content <- look "catContent"
     liftIO $ changeCategory _id
       ( Category { catId = _id
                  , catTitle = title
                  , catName = name
                  , catContent = content
            })
     jsonResponse $ encodeJSON ("ok" ::String)

jsonCategories :: ServerPart Response
jsonCategories =
  do res <- liftIO $ getCategories
     jsonResponse $ encodeJSON $ fmap markCategory res


jsonCheckLogin :: ServerPart Response
jsonCheckLogin =
  do checkAuth
     jsonResponse $ encodeJSON ("ok" ::String)

jsonLogin :: ServerPart Response
jsonLogin =
  do methodM POST
     liftIO $ putStrLn "LOGIN!!!"
     pass <- look "pass"
     res <- liftIO $ checkUserPass "admin" pass
--     when (res)
     (addCookie (MaxAge (3600*24*7)) (mkCookie "pass" pass))
     jsonResponse $ encodeJSON res

jsonLogout :: ServerPart Response
jsonLogout =
  do expireCookie "pass"
     expireCookie "login"
     jsonResponse $ encodeJSON ("ok" ::String)

readNumOr :: String -> Integer -> Integer
readNumOr str or =
  case a of
    [] -> or
    [(num,_)] -> num
  where a = reads str

jsonAddItem :: ServerPart Response
jsonAddItem =
  do checkAuth
     methodM POST
     title <- look "title"
     description <- look "description"
     category <- look "category"
     cost <- (filter isNumber) `liftM` look "cost"
     tags <- look "tags"
     old_cost <- (filter isNumber) `liftM` look "old_cost"
     image1 <- optional $ lookFile "image1"
     date <- show `liftM` liftIO getCurrentTime

     _id <- liftIO $ addItem
       (Item "" title date description category (readNumOr cost 0) (readNumOr old_cost 0) (parseTags tags) Nothing)
     liftIO $ print image1
     case image1 of
       Just (_, "", _) -> return ()
       Just (filename, origFilename, ContentType mainType subType _) ->
         liftIO $ uploadAndConvert _id filename
       Nothing -> return ()
     jsonResponse $ encodeJSON ("ok" ::String)

jsonDelItem :: ServerPart Response
jsonDelItem =
  do checkAuth
     methodM POST
     _id <- look "id"
     liftIO $ deleteItem _id
     jsonResponse $ encodeJSON ("ok" ::String)

jsonGetImage str =
  do methodM GET
     id <- look "id"
     image <- look "image"
     (content, contentType) <- liftIO $ getImage (fromString (id)) (UTF8S.fromString image)
     ok $ toResponseBS (contentType) (BL.pack $ B.unpack content)


jsonDelImage =
  do checkAuth
     methodM POST
     _id <- look "id"
     image <- look "image"
     liftIO $ print $ "DEL IMAGE ROUTE..." ++ image ++ _id
     liftIO $ delImage (fromString _id) (UTF8S.fromString image)
     jsonResponse $ encodeJSON ("ok" ::String)

jsonUploadImage =
  do checkAuth
     methodM POST
     liftIO $ print "UPLOAD ROUTE..."
     _id <- look "id"
     name <- look "name"
     (filename, origFilename, ContentType mainType subType _) <- lookFile "image"
     liftIO $ uploadAndConvert (fromString _id) filename
     jsonResponse $ encodeJSON ("ok" ::String)

jsonAddCategory :: ServerPart Response
jsonAddCategory =
  do checkAuth
     methodM POST
     name <- look "catName"
     title <- look "catTitle"
     liftIO $ addCategory
       (Category { catName  = name
                 , catId = ""
                 , catContent = ""
                 , catTitle = title
                 })
     jsonResponse $ encodeJSON ("ok" ::String)

jsonGetCategory :: ServerPart Response
jsonGetCategory =
  do id <- look "id"
     res <- liftIO $ getCategory id
     jsonResponse $ encodeJSON res

jsonDeleteCategory :: ServerPart Response
jsonDeleteCategory =
  do checkAuth
     methodM POST
     cat <- look "cat"
     liftIO $ deleteCategoryWithContent cat
     jsonResponse $ encodeJSON ("ok" ::String)

jsonCheckout :: ServerPart Response
jsonCheckout =
  do methodM POST
     name <- look "requestName"
     telephone <- look "requestTelephone"
     description <- look "requestDescription"
     liftIO $ print "DESC"
     items <- splitOn "," `liftM` look "items"
     liftIO $ print "items"
     counts <- splitOn "," `liftM` look "counts"
     liftIO $ print "counts"

     liftIO $ print "phase1"
     now <- liftIO getCurrentTime
     liftIO $ print "phase2"
     dbItems <- liftIO $ catMaybes `liftM` mapM getItem items
     liftIO $ print "phase3"
     let costs = map (show . cost) dbItems
     liftIO $ print "phase4"
     let names = map (title) dbItems
     let rItems = zipWith4 (RequestItem) items names costs counts
     _ <- liftIO $ addRequest
       (Request { requestName  = name
                , requestId = ""
                , requestTelephone = telephone
                , requestDescription = description
                , requestDate = now
                , requestNew = True
                , requestItems = rItems
                })
     jsonResponse $ encodeJSON ("ok" ::String)

jsonNewRequests :: ServerPart Response
jsonNewRequests =
  do checkAuth
     res <- liftIO $ newRequests
     jsonResponse $ encodeJSON res

jsonRequests :: ServerPart Response
jsonRequests =
  do checkAuth
     res <- liftIO $ allFromRequests
     jsonResponse $ encodeJSON res


jsonFeedbacks :: ServerPart Response
jsonFeedbacks =
  do res <- liftIO $ getFeedbacks True
     jsonResponse $ encodeJSON res

jsonReadRequests :: ServerPart Response
jsonReadRequests =
  do checkAuth
     res <- liftIO $ newRequests
     let readed = map (\a -> a {requestNew = False}) res
     liftIO $ mapM_ (\a -> changeRequest (requestId a) a) readed
     jsonResponse $ encodeJSON ("ok" ::String)

jsonOldRequests :: ServerPart Response
jsonOldRequests =
  do checkAuth
     res <- liftIO $ oldRequests
     jsonResponse $ encodeJSON res


checkAuth = do liftIO $ print "CHECK AUTH ROUTE..."
               pass1 <- optional $ lookCookieValue "pass"
               liftIO $ print pass1
               pass <- lookCookieValue "pass"
               ret <- lookDbUser "admin" pass
               liftIO $ print $ "AUTH complete: " ++ show ret
               return ret

lookDbUser login pass =
  do a <- liftIO $ checkUserPass login pass
     case a of
       True -> return True
       _ -> mzero



myPolicy :: BodyPolicy
myPolicy = (defaultBodyPolicy "/tmp/" (15*10^6) 5000000 5000000)

main = do putStrLn $ "Started: *:" ++ show webPort
          prepareDb
          simpleHTTP nullConf {port = fromIntegral webPort} $
            do decodeBody myPolicy
               msum [dir "json" $ msum
                     [ dir "category" $ jsonFromCategory True
                     , dir "categorytag" $ jsonFromCategoryTag
                     , dir "categorycost" $ jsonFromCategory False
                     , dir "tagscategory" $ jsonCategoryTags
                     , dir "getcategory" $ jsonGetCategory
                     , dir "delcategory" $ jsonDeleteCategory
                     , dir "checklogin" $ jsonCheckLogin
                     , dir "login" $ jsonLogin
                     , dir "categories" $ jsonCategories
                     , dir "item" $ jsonItem
                     , dir "edit" $ jsonEdit
                     , dir "editcategory" $ jsonEditCategory
                     , dir "uploadimage" $ jsonUploadImage
                     , dir "delimage" $ jsonDelImage
                     , dir "searchitem" $ jsonSearchItem
                     , dir "logout" $ jsonLogout
                     , dir "content" $ jsonContent
                     , dir "rawcontent" $ jsonRawContent
                     , dir "editcontent" $ jsonEditContent
                     , dir "requests" $ jsonRequests
                     , dir "oldrequests" $ jsonOldRequests
                     , dir "newrequests" $ jsonNewRequests
                     , dir "feedbacks" $ jsonFeedbacks
                     , dir "readrequests" $ jsonReadRequests
                     , dir "additem" $ jsonAddItem
                     , dir "delitem" $ jsonDelItem
                     , dir "image" $ jsonGetImage "1.jpeg"
                     , dir "checkout" $ jsonCheckout
                     , dir "addcat" $ jsonAddCategory
                     , jsonResponse $ encodeJSON ("fail" ::String)]
                    , serveDirectory EnableBrowsing ["index.html"] "static"
                    , serveFile (guessContentTypeM mimeTypes) "404.html"]
