{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Wreq
import Control.Lens
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U8
import Control.Monad (replicateM_)

import System.IO
import Control.Concurrent(threadDelay)

main :: IO ()
main = do
  f <- openFile "/Users/mgh/imgs/codes.txt" WriteMode
  replicateM_ 10 $ do
    r<- get "http://bus.kuaizhan.com/auth/api/sms/picvcode.jpg?1479376496872"
    let device_id = r ^. responseCookie "device_id" . cookieValue
        content = r ^. responseBody
    L.writeFile (U8.toString (S.concat ["/Users/mgh/imgs/",device_id,".jpg"])) content
    hPutStrLn f (U8.toString (S.concat [device_id," "]))
  hClose f

sendRequest :: IO ()
sendRequest = do 
    f <- readFile "/Users/mgh/imgs/codes.txt"
    mapM_ sendOne $ toTuple f    
    where
      toTuple x = map (\l -> (l!!0,l!!1))  $ map words $ lines x
      sendOne (deviceId,code) = do
        let opts = defaults & header "Cookie" .~ [U8.fromString $ "device_id=" ++ deviceId]
        resp <- getWith opts $ "http://bus.kuaizhan.com/bus/1.0/apps/55d45b2dde0f01bf5ba98dcf/env/pro/funcs/sohu_multivote?site_id=3755165334&data=58213bccbeacc01b73dfa6a2&uid=weNpJVmk&code=" ++ code
        putStrLn $ show $ resp ^. responseBody
        threadDelay 200

go :: String -> IO ()
go s = do 
    mapM_ sendOne $ toTuple s    
    where
      toTuple x = map (\l -> (l!!0,l!!1))  $ map words $ lines x
      sendOne (deviceId,code) = do
        let opts = defaults & header "Cookie" .~ [U8.fromString $ "device_id=" ++ deviceId]
        resp <- getWith opts $ "http://bus.kuaizhan.com/bus/1.0/apps/55d45b2dde0f01bf5ba98dcf/env/pro/funcs/sohu_multivote?site_id=3755165334&data=58213bccbeacc01b73dfa6a2&uid=weNpJVmk&code=" ++ code
        putStrLn $ show $ resp ^. responseBody
        threadDelay 200

s = do
  s <- readFile "/Users/mgh/imgs/codes.txt"
  mapM_ (\_-> go s) [1..150]

