-- Main
import GetData

main :: IO ()
main = do
    name <- getName
    host <- getHost
    os <- getOs "/etc/os-release"
    kver <- getKrnlV
    sh <- getShell
    wm <- getWm
    theme <- getTheme
    term <- getTerm
    ip <- getIp
    cpu <- getCpu
    ram <- getRam

    putStrLn $ name ++ "@" ++ host
    putStrLn "-------------"
    putStrLn $ "OS: " ++ os
    putStr $ "Kernel: " ++ kver
    putStrLn $ "Shell: " ++ sh
    putStrLn "-------------"
    putStrLn $ "WM: " ++ wm
    putStrLn $ "Theme: " ++ theme
    putStrLn $ "Terminal: " ++ term
    putStrLn "-------------"
    putStrLn $ "Local IP: " ++ ip
    putStrLn $ "CPU Usage: " ++ cpu ++ "%"
    putStrLn $ "RAM Usage: " ++ ram
