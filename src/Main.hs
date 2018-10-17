{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import GHC.Generics
import qualified Data.Aeson as A
import Control.Applicative
import Snap.Core
import Snap.Util.FileServe
import Snap.Snaplet
import Snap.Http.Server
import Lang.Parser

data MainService = MainService

data CodeParseRequest =
  CodeParseRequest
  { codeText :: String
  } deriving ( Show
             , Generic
             )

instance A.FromJSON CodeParseRequest

data CodeParseResp =
  CodeParseResp
  { code :: String
  } deriving ( Show
             , Generic
             )

instance A.ToJSON CodeParseResp

siteRoutes :: [(BS.ByteString, Handler b MainService ())]
siteRoutes =
    [ ("/parse", method POST handleParseRequest)
    ]

handleParseRequest :: Handler b MainService ()
handleParseRequest = do
  body <- readRequestBody 102400
  modifyResponse $ setHeader "Content-Type" "application/json"
  case (A.decode body :: Maybe CodeParseRequest) of
    Just codeParseRequest ->
      let parsed = runParseLang (codeText codeParseRequest)
      in do
        writeLBS (A.encode $ CodeParseResp (show parsed))
        modifyResponse $ setResponseCode 200
    Nothing ->
      modifyResponse $ setResponseCode 400


config :: Config Snap a
config =
  setErrorLog (ConfigIoLog print) $
  setAccessLog (ConfigIoLog print) $
  setPort 8080 defaultConfig

mainServiceInit :: SnapletInit a MainService
mainServiceInit = makeSnaplet "code-report" "Code Report App" Nothing $ do
  addRoutes siteRoutes
  return MainService

main :: IO ()
main =
  serveSnaplet config mainServiceInit
