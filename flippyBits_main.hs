module FlippyBits where

import Data.IORef

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

import Data.IORef
import Data.List

import CheckAns
import qualified RandNumGen as RAND
import BaseConvert

list00000000:: [String]
list00000000 = ["0","0","0","0","0","0","0","0"]

go:: IO ()
go = do
  putStrLn "Welcome to Flippy Bits!"
  putStrLn "Please choose the base you'd want to play in:"
  b <- getLine
  st <- start list00000000 (read b::Int)
  return st


start:: [String] -> Int -> IO ()
start inputArr b = startGUI defaultConfig { jsStatic = Just "." } (setup inputArr b)

setup :: [String] -> Int -> Window -> UI ()
setup inputArr b window = do
  return  window # set UI.title "Flippy Bits"
  randStrP <- liftIO (randN b)
  buttons <- mkButtons inputArr randStrP b
  let prompt = UI.h1 #+ [ string ("Convert the given number from base " ++ show b ++ " to binary!") ]
  getBody window #+ [prompt, mkHexNum randStrP]
  getBody window #+ [UI.div #. "wrap" #+ (map element buttons)]
  return ()

mkButton:: [String] -> IORef [Int] -> (String,Int) -> Int -> Int -> UI Element
mkButton arr d0 strP b n = do
  button <- UI.button #+ [string (arr !! n)]
  on UI.click button $ \_ -> do
      x <- liftIO $ do
          modifyIORef d0 (modifyD0 n)
          readIORef d0
      element button # set UI.text (show (x!!n)::String)
      if checkans_binStr_decInt (foldl (\h t -> h*10+t) 0 x) (snd strP)
        then do
          window <- liftUI askWindow
          winBody <- liftUI (getBody window)
          UI.delete winBody
          setup arr b window
          return button
        else return button
  return button

mkButtons:: [String] -> (String,Int) -> Int -> UI [Element]
mkButtons arr strP b = do
  d0 <- liftIO $ newIORef [0,0,0,0,0,0,0,0]
  dig0 <- mkButton arr d0 strP b 0
  dig1 <- mkButton arr d0 strP b 1
  dig2 <- mkButton arr d0 strP b 2
  dig3 <- mkButton arr d0 strP b 3
  dig4 <- mkButton arr d0 strP b 4
  dig5 <- mkButton arr d0 strP b 5
  dig6 <- mkButton arr d0 strP b 6
  dig7 <- mkButton arr d0 strP b 7
  return [dig0, dig1, dig2, dig3, dig4, dig5, dig6, dig7]

mkHexNum:: (String, Int) -> UI Element
mkHexNum strP = do
  hexN <- UI.h1  #+ [string (fst strP)]
  return hexN

modifyD0:: Int -> [Int] -> [Int]
modifyD0 n d0 = (if (d0!!n)==0 then (replaceN 1 0 n d0) else (replaceN 0 0 n d0))

replaceN:: Int -> Int -> Int -> [Int] -> [Int]
replaceN d ini dest (h:t) =
  if (ini==dest) then d:t else h:(replaceN d (ini+1) dest t)

randN:: Int -> IO (String, Int)
randN b = do
  n <- RAND.randRange 1 255
  let n2 = convert n b []
  return ((reverse n2), n)
