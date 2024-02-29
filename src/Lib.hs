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

printReponseBody :: Response L8.ByteString -> IO ()
printReponseBody response = do
    putStrLn "Response Body:"
    case decode responseBody :: Maybe Value of
        Just v  -> print v
        Nothing -> putStrLn "Failed to decode response body"
    where
        responseBody = getResponseBody response

printResponse :: ResponseHeaders -> IO ()
printResponse headers = do
    putStrLn "Response Headers:"
    mapM_ print headers

someFunc :: IO ()
someFunc = do
    putStrLn "Enter a URL:"
    url <- getLine
    case parseRequest url of
        Just _ -> do
            request <- parseRequest url
            response <- httpLBS request
            putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
            let responseType = lookup hContentType (getResponseHeaders response)

            putStrLn $ "The response type is: " ++ show responseType
            printResponse (getResponseHeaders response)
            if responseType == Just (B8.pack "application/json")
                then do
                  putStrLn "The response type is JSON"
                  printReponseBody response
                else print $ show (getResponseBody response)
            
        Nothing -> do
            putStrLn $ "Invalid URL: " ++ url
            someFunc
