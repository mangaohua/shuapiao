{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Wreq
import Control.Lens
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import System.IO

setCookie :: S.ByteString
setCookie = "device_id=pyiIDQya"

opts = defaults & header "Cookie" .~ [setCookie]

code = "lu84"
resp = getWith opts $ "http://bus.kuaizhan.com/bus/1.0/apps/55d45b2dde0f01bf5ba98dcf/env/pro/funcs/sohu_multivote?site_id=3755165334&data=58213bccbeacc01b73dfa6a2&uid=weNpJVmk&code=" ++ code



main :: IO ()
main = do
  r<- get "http://bus.kuaizhan.com/auth/api/sms/picvcode.jpg?1479376496872"
  let device_id = r ^. responseCookie "device_id" . cookieValue
      content = r ^. responseBody
  L.writeFile (show device_id) content


