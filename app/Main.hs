{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Text
import Test.WebDriver 
import Test.WebDriver.Session
import Test.WebDriver.Commands
import Test.WebDriver.Commands.Wait
import Test.WebDriver.JSON (ignoreReturn)
import qualified Config as C
type User = Text

chromeConfig :: WDConfig
chromeConfig = useBrowser chr defaultConfig
  { wdHost = "0.0.0.0", wdPort = 4444, wdHTTPRetryCount = 50 }
  where chr = chrome

session = runSession chromeConfig $ getSession
  
loginUMP :: WD Text
loginUMP = do
  (openPage "https://nexmo-omni-channel.geeks-lab.net/cs/ump")
  (findElem (ById "apiSecret")) >>= sendKeys C.apiKey
  (findElem (ById "apiKey")) >>= sendKeys C.apiSecret
  (findElem (ByXPath "//button[contains(text(),Login)]")) >>= click
  (findElem (ByXPath "//span[contains(text(),'Your applications')]")) >>= click
  (waitUntil 5 (findElem $ ByXPath $ "//span[contains(text(),'" <> C.appId <> "')]"))>>=getText

sendMessage :: User -> WD()
sendMessage user = do
  (openPage "https://nexmo-omni-channel.geeks-lab.net/cs/app")
  (findElem (ById "username")) >>= sendKeys user
  (findElem (ById "password")) >>= sendKeys C.globalPassword
  (findElem (ById "appId")) >>= sendKeys C.appId
  (findElem (ByXPath "//button[contains(text(),Login)]")) >>= click
  (waitUntil 10 (findElem (ByXPath "//input[contains(@placeholder,'Type a message')]"))) >>= sendKeys "Some data\n"

    
main :: IO ()
main = session >>= flip runWD (loginUMP>>sendMessage "u1">>sendMessage "u2")
