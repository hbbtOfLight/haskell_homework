{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import User
import Web.Scotty
import Network.HTTP.Types
import Data.IORef
import Data.Text.Lazy (pack)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Environment
import System.Console.CmdArgs

formatEndpoint (path, description) = path ++ ": " ++ description

formatEndpoints = unlines . map formatEndpoint


data PortArg = PortArg {
    port :: Int
} deriving (Show, Data, Typeable)

main :: IO ()
main = do  
  op <- cmdArgs PortArg {port = 3000}
  storeRef <- newIORef Map.empty
  scotty (port op) $ do
    get "/" $ do
      text "HELLO! I'm a simple key-value web server! To get some help call /help!"

    get "/help" $ do
      let endpoints =
            [ ("/", "Returns a hello message.")
            , ("/help", "Returns information about available endpoints.")
            , ("/value/:k", "Returns the user by key (GET) or deletes it (DELETE)")
            , ("/key", "Adds a user to the key-value map, their username used as key, content-type=json")
            , ("/list/:limit", "takes :limit (random?) users from map")
            ]

      let helpMessage = formatEndpoints endpoints

      text (pack helpMessage)

    get "/value/:k" $ do
      username <- param "k"
      storage <- liftAndCatchIO $ readIORef storeRef
      let user = Map.lookup username storage
      case user of
        Just u -> json u
        Nothing -> do
          status status404
          text (pack("Not found: " ++ username))
    
    delete "/value/:k" $ do
      username <- param "k"
      storage <- liftAndCatchIO $ readIORef storeRef
      liftAndCatchIO $ modifyIORef' storeRef (Map.delete (username))

    
    get "/list/:limit" $ do
      limit <- param "limit"
      storage <- liftAndCatchIO $ readIORef storeRef
      let users = take limit $ Map.elems storage
      json users


    post "/key" $ do
      user <- jsonData :: ActionM (Maybe User)
      case user of
        Just u -> do
          liftAndCatchIO $ modifyIORef' storeRef (Map.insert (userName u) u)
          text (pack("Added " ++ (userName u)))
        Nothing -> do
          status status400
          text "Bad data" 


    notFound $ do
      status status404
      text "Unavailable root"