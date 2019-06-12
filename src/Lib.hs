{-# LANGUAGE OverloadedStrings #-}
module Lib ( checkOmni ) where
import Data.Time
import Data.Text
import Test.WebDriver 
import Test.WebDriver.Session
import Test.WebDriver.Commands
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)
import qualified Config as C
import Control.Concurrent.Async

type User = Text

chromeConfig :: WDConfig
chromeConfig = useBrowser chr defaultConfig
  { wdHost = "0.0.0.0", wdPort = 4444, wdHTTPRetryCount = 50 }
  where chr = chrome

session = runSession chromeConfig $ getSession
  
loginUMP :: WD ()
loginUMP = do
  (openPage "https://nexmo-omni-channel.geeks-lab.net/cs/ump")
  (findElem (ById "apiSecret")) >>= sendKeys C.apiKey
  (findElem (ById "apiKey")) >>= sendKeys C.apiSecret
  (findElem (ByXPath "//button[contains(text(),Login)]")) >>= click
  (findElem (ByXPath "//span[contains(text(),'Your applications')]")) >>= click
  elem <- (waitUntil 5 (findElem $ ByXPath $ "//span[contains(text(),'" <> C.appId <> "')]"))
  return ()


sendMessage :: User -> Text -> User -> WD()
sendMessage user msg toUser = do
  (openPage "https://nexmo-omni-channel.geeks-lab.net/cs/app")
  (findElem (ById "username")) >>= sendKeys user
  (findElem (ById "password")) >>= sendKeys C.globalPassword
  (findElem (ById "appId")) >>= sendKeys C.appId
  (findElem (ByXPath "//button[contains(text(),Login)]")) >>= click
  (waitUntil 10 (findElem (ByXPath "//input[contains(@placeholder,'Type a message')]"))) >>= sendKeys (msg <>"from " <> user <> "\n")
  (waitUntil 10 (findElem (ByXPath ("//b[contains(text(),'" <> (msg <>"from " <> toUser) <>"')]"::Text)))) >>= click
  
checkOmni :: IO()
checkOmni = do
  time <- (pack.show)<$> getCurrentTime
  all <- mapConcurrently (\wd -> do
                             s <- session
                             runWD s wd
                             return s) [loginUMP,sendMessage "u1" time "u2" ,sendMessage "u2" time "u1"]
  mapM (flip runWD closeSession) all
  return ()
