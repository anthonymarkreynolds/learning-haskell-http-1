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


-- decodeResponseBody :: Response ByteString -> Maybe Value
-- decodeResponseBody response = decode $ fromStrict $ getResponseBody response

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
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLBS request
    putStrLn $ "The status code was: " ++ show (getResponseStatusCode response)
    let responseType = lookup hContentType (getResponseHeaders response)
    putStrLn $ "The response type is: " ++ show responseType
    -- let responseBody = getResponseBody response
    
    printResponse (getResponseHeaders response)
    -- putStrLn $ "Response Body: " ++ show (decode responseBody :: Maybe Value)
    printReponseBody response
