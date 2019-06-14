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
type AppId = Text
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
  (waitUntil 3 (findElem (ByXPath "//span[contains(text(),'New application')]")))
  return ()

createApp :: WD()
createApp = do
  (findElem (ByXPath "//span[contains(text(),'New application')]")) >>= click
  (findElem (ById "appName")) >>= sendKeys "Monitor"
  (findElem (ByXPath "(//label/span)[2]")) >>= click
  (findElem (ByXPath "//span/button[contains(text(),'Next')]")) >>= click
  (waitUntil 10 (findElem (ById "defaultPassword")))
  return()

configureApp :: WD()
configureApp = do 
  (findElem (ById "defaultPassword")) >>= sendKeys C.globalPassword
  (findElem (ByXPath "//span/button[contains(text(),'Next')]")) >>= click
  (waitUntil 10 (findElem (ByXPath "//h3[contains(text(),'Your Users')]")))
  return ()
  
createUsers :: WD()
createUsers = do
  (findElem (ById "name")) >>= sendKeys "u1"
  (findElem (ByXPath "(//button)[2]")) >>= click
  (findElem (ByXPath "(//input[@id='name'])[2]")) >>= sendKeys "u2"
  (findElem (ByXPath "(//button)[4]")) >>= click
  (findElem (ByXPath "//span/button[contains(text(),'Next')]")) >>= click
  (waitUntil 10 (findElem (ByXPath "//h3[contains(text(),'Your conversations')]")))
  return ()


createConversation :: WD()
createConversation = do
  (findElem (ByXPath "(//input[@id='name'])[1]")) >>= sendKeys "c"
  (findElem (ByXPath "(//div[@id=0]/button)")) >>= click
  (findElem (ByXPath "//span/button[contains(text(),'Next')]")) >>= click
  (waitUntil 10 (findElem (ByXPath "//h3[contains(text(),'Assign Users to Conversations')]")))
  return ()
  

assignUsersToC :: WD(Text)
assignUsersToC = do
  (findElem (ById "wToggleDropdownButton")) >>= click
  (findElem (ById "w-select-all-users-btn")) >>= click
  (findElem (ById "wAddSelectedUsers")) >>= click
  (findElem (ByXPath "//span/button[contains(text(),'Finish')]")) >>= click
  (waitUntil 50 (findElem (ByXPath "//span[contains(text(),'Launch the web app')]")))
  (findElem (ByXPath "//div/div[2]/div/div/div[2]")) >>= getText
  
createNewApp :: WD(Text)
createNewApp = do
  loginUMP
  createApp
  configureApp
  createUsers
  createConversation
  assignUsersToC


sendMessage :: AppId -> User -> Text -> User -> WD()
sendMessage appId user msg toUser = do
  (openPage "https://nexmo-omni-channel.geeks-lab.net/cs/app")
  (findElem (ById "username")) >>= sendKeys user
  (findElem (ById "password")) >>= sendKeys C.globalPassword
  (findElem (ById "appId")) >>= sendKeys appId
  (findElem (ByXPath "//button[contains(text(),Login)]")) >>= click
  (waitUntil 10 (findElem (ByXPath "//input[contains(@placeholder,'Type a message')]"))) >>= sendKeys (msg <>"from " <> user <> "\n")
  (waitUntil 10 (findElem (ByXPath ("//b[contains(text(),'" <> (msg <>"from " <> toUser) <>"')]"::Text)))) >>= click
  
checkOmni :: IO()
checkOmni = do
  time <- (pack.show)<$> getCurrentTime
  admin <- session
  appId <- runWD admin createNewApp
  all <- mapConcurrently (\wd -> do
                             s <- session
                             runWD s wd
                             return s) [ sendMessage appId "u1" time "u2" ,sendMessage appId "u2" time "u1"]
  mapConcurrently (flip runWD closeSession) (admin:all)
  return ()
