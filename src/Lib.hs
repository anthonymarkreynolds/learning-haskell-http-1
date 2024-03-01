{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    ) where

import           Network.HTTP.Simple       (Response, getResponseBody,
                                            getResponseHeaders,
                                            getResponseStatusCode, httpLBS,
                                            parseRequest)
import           Network.HTTP.Types.Header (ResponseHeaders, hContentType)
import           Data.Aeson                (decode)
import           Data.Aeson.Types          (Value)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT, logInfo)
import qualified Data.Text as T

-- | Show a value as Text
tshow :: Show a => a -> T.Text
tshow = T.pack . show    

printReponseBody :: Response L8.ByteString -> LoggingT IO ()
printReponseBody response = do
    $(logInfo) $ "Response Body (from logger): " <> tshow responseBody
    liftIO $ putStrLn "Response Body:"
    case decode responseBody :: Maybe Value of
        Just v  -> liftIO $ print v
        Nothing -> liftIO $ putStrLn "Failed to decode response body"
    where
        responseBody = getResponseBody response

printResponse :: ResponseHeaders -> IO ()
printResponse headers = do
    putStrLn "Response Headers:"
    mapM_ print headers

someFunc :: LoggingT IO ()
someFunc = do
    $(logInfo) "Enter a URL:"
    url <- liftIO getLine
    case parseRequest url of
        Just request -> do
            response <- httpLBS request
            $(logInfo) $ "The status code was: " <> tshow (getResponseStatusCode response)
            let responseType = lookup hContentType (getResponseHeaders response)

            $(logInfo) $ "The response type is: " <> tshow responseType
            liftIO $ printResponse (getResponseHeaders response)
            if responseType == Just (B8.pack "application/json")
                then do
                    $(logInfo) "The response type is JSON"
                    printReponseBody response
                else liftIO $ print $ show (getResponseBody response)
        Nothing -> do
            $(logInfo) $ "Invalid Url: " <> T.pack url
            someFunc