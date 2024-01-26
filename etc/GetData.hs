-- Getting data
module GetData where
import System.IO
import System.Environment (getEnv, lookupEnv)
import System.Info (os)
import System.Process (readProcess)
import Data.List (find, isInfixOf)

formLine :: String -> String
formLine line = let
   trimmed = drop 12 line
   cleaned = filter (/= '"') trimmed
   in cleaned

getOs :: FilePath -> IO String
getOs filePath = do
   content <- readFile filePath
   return (formLine (head (lines content)))

getName :: IO String
getName = getEnv "USER"

getHost :: IO String
getHost = do
   maybeHost <- lookupEnv "HOSTNAME"
   case maybeHost of
      Just host -> return host
      Nothing   -> return "localhost"

getKrnlV :: IO String
getKrnlV = do
   output <- readProcess "uname" ["-r"] ""
   return (trim output)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile (== ' ')

getShell :: IO String
getShell = do
   shell <- getEnv "SHELL"
   return shell

getWm :: IO String
getWm = do
   maybeDesktop <- lookupEnv "XDG_CURRENT_DESKTOP"
   maybeWm <- case maybeDesktop of
      Just desktop -> return (Just desktop)
      Nothing      -> lookupEnv "WINDOW_MANAGER"

   case maybeWm of
      Just wm -> return wm
      Nothing -> return "Unknown"

getTheme :: IO String
getTheme = do
   maybeTheme <- lookupEnv "GTK_THEME"
   case maybeTheme of
      Just theme -> return theme
      Nothing    -> return "Default"

getTerm :: IO String
getTerm = getEnv "TERM"

getIp :: IO String
getIp = do
   output <- readProcess "hostname" ["-I"] ""
   return $ head $ words output 

getCpu :: IO String
getCpu = do
    output <- readProcess "top" ["-bn1"] ""
    let cpuLine = head . filter ("Cpu(s)" `isInfixOf`) $ lines output
    let cpuUsage = filter (/= ',') $ words cpuLine !! 1
    return $ init cpuUsage

getRam :: IO String
getRam = do
    output <- readProcess "free" ["-m"] ""
    let memoryL = take 2 . drop 1 . lines $ output
    case words (head memoryL) of
        ["total", total, "used", used, _, _] -> return $ used ++ " MB / " ++ total ++ " MB"
        _ -> return "Unknown"

-- Amen.
