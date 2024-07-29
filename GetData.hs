module GetData where

import System.Posix.User (getEffectiveUserName)
import System.Process (readProcess)
import System.IO (readFile)
import Data.List (isPrefixOf, words)
import System.Info (os, arch)
import Control.Exception (catch, IOException)
import System.Environment (lookupEnv)

-- Get username
getName :: IO String
getName = getEffectiveUserName

-- Get architecture
getArch :: IO String
getArch = return arch

-- Get hostname
getHost :: IO String
getHost = do
  hostName <- readProcess "hostname" [] ""
  return (filter (/= '\n') hostName)

-- Get OS (GNU/Linux)
getLinux :: IO String
getLinux = do
  distro <- readProcess "lsb_release" ["-d"] ""
  return (filter (/= '\n') $ drop 11 distro)
  `catch` handleError
  where
    handleError :: IOException -> IO String
    handleError _ = return "Custom Linux Distribution"

-- Get OS (GNU/Linux) Alternative
getLinuxA :: IO String
getLinuxA = do
  contents <- readFile "/etc/os-release"
  return $ case filter (("NAME=" `isPrefixOf`) . strip) (lines contents) of
    (line:_) -> drop 5 $ line
    []       -> "Custom Linux Distribution"
  where
    strip = filter (/= '\n')

-- Get OS (*BSD)
getBsd :: IO String
getBsd = do
  osName <- readProcess "uname" ["-s"] ""
  return (filter (/= '\n') osName)
  `catch` handleError
  where
    handleError :: IOException -> IO String
    handleError _ = return "Custom BSD"

-- Selecting OS
select :: IO String
select = case os of
  "Linux"   -> getLinux `catch` (\(_ :: IOException) -> getLinuxA)
  "freebsd" -> getBsd
  "openbsd" -> getBsd
  "netbsd"  -> getBsd
  _         -> return "Custom OS"

-- Get Kernel version
getKrnl :: IO String
getKrnl = do
  kernel <- readProcess "uname" ["-r"] ""
  return (filter (/= '\n') kernel)

-- Get shell
getShell :: IO String
getShell = do
  shell  <- lookupEnv "SHELL"
  return $ case shell of
    Just shell -> shell
    Nothing    -> "Custom Shell"

-- Get WM
wmEnv :: [String]
wmEnv = ["DESKTOP_SESSION", "XDG_CURRENT_DESKTOP", "GDMSESSION", "XDG_SESSION_DESKTOP"]

getWm :: IO String
getWm = do
  wm <- findWm wmEnv
  return $ case wm of
    Just wmName -> wmName
    Nothing     -> "Custom WM"

findWm :: [String] -> IO (Maybe String)
findWm [] = return Nothing
findWm (var:vars) = do
  value <- lookupEnv var
  case value of
    Just val -> return (Just val)
    Nothing  -> findWm vars

-- Amen
