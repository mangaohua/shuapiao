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
import Control.Exception as E
import Network.HTTP.Client(HttpException( StatusCodeException ))
main :: IO ()
main = do
  f <- openFile "/Users/mgh/imgs/codes.txt" WriteMode
  mapM_ (\i -> do
    r<- get "http://bus.kuaizhan.com/auth/api/sms/picvcode.jpg?1479376496872"
    let device_id = r ^. responseCookie "device_id" . cookieValue
        content = r ^. responseBody
    S.writeFile (U8.toString (S.concat ["/Users/mgh/imgs/",U8.fromString (show i),"_",device_id,".jpg"])) (L.toStrict content)
    hPutStrLn f (U8.toString (S.concat [device_id," "]))
    hFlush f ) [1..20]
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
        putStrLn $ ((show $ resp ^. responseBody) ++ deviceId)
        threadDelay 100

go :: String -> IO ()
go s = do 
    mapM_ sendOne $ toTuple s    
    where
      toTuple x = map (\l -> (l!!0,l!!1))  $ map words $ lines x
      sendOne (deviceId,code) = do
        resp <- (getWith opts $ "http://bus.kuaizhan.com/bus/1.0/apps/55d45b2dde0f01bf5ba98dcf/env/pro/funcs/sohu_multivote?site_id=3755165334&data=58213bccbeacc01b73dfa6a2&uid=weNpJVmk&code=" ++ code) `E.catch` handler
        putStrLn $ show $ resp ^. responseBody
        threadDelay 100
        where handler (StatusCodeException s _ _) = getWith opts $ "http://bus.kuaizhan.com/bus/1.0/apps/55d45b2dde0f01bf5ba98dcf/env/pro/funcs/sohu_multivote?site_id=3755165334&data=58213bccbeacc01b73dfa6a2&uid=weNpJVmk&code=" ++ code
              opts = defaults & header "Cookie" .~ [U8.fromString $ "device_id=" ++ deviceId]
s = do
  s <- readFile "/Users/mgh/imgs/codes.txt"
  mapM_ (\_-> go s) [1..180]

