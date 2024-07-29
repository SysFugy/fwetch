import GetData

import System.IO (readFile)
import Data.List (transpose)

-- Here your ASCII art
asciiArt :: String
asciiArt = unlines
  [ "X---------X"
  , "| |  |--- |"
  , "| ---|--- |"
  , "|    |  | |"
  , "| ---|  | |"
  , "X---------X"
  ]


main :: IO ()
main = do
  name <- getName
  arch <- getArch
  host <- getHost
  os   <- select 
  krnl <- getKrnl
  shel <- getShell
  wm   <- getWm

  -- Create separator
  let nhLen = length name + length host + 1
      separator = replicate nhLen '-'

  let asciiLines = lines asciiArt
  
  let maxArtLen = maximum (map length asciiLines)
      offset = 2
      totalLen = maxArtLen + offset

  -- Information about the system from GetData.hs
  let infoLines = [ name ++ "@" ++ host
              , separator
              , "OS: " ++ os ++ " " ++ arch
              , "Kernel: " ++ krnl
              , "Shell: " ++ shel
              , "WM: " ++ wm  
              ]

  -- ASCII art 
  let formatLine art info = art ++ replicate (totalLen - length art) ' ' ++ info
      combinedLines = zipWith formatLine asciiLines (infoLines ++ repeat "")

  putStr (unlines combinedLines)
