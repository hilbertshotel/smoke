module Error where

err = "\x1b[31merror:\x1b[0m"

unknown :: String -> IO ()
unknown sub = putStrLn $ err ++ " unknown subcommand `" ++ sub ++ "`"

many :: IO ()
many = putStrLn $ err ++" too many arguments"

missing :: String -> IO ()
missing path = putStrLn $ err ++ " could not find `" ++ path ++ "`"

exists :: String -> IO ()
exists name = putStrLn $ err ++ " `" ++ name ++ "` already exists"

noname :: String -> IO ()
noname cmd = putStrLn $ err ++ " subcommand `" ++ cmd ++ "` requires a <name> argument"

noargs :: String -> IO ()
noargs cmd = putStrLn $ err ++ " `" ++ cmd ++ "` takes no arguments"

git :: IO ()
git = putStrLn $ err ++ " failed to initialize `git`"

ghc :: IO ()
ghc = putStrLn $ err ++ " `ghc` is not installed on the system"

execPath :: IO ()
execPath = putStrLn $ err ++ " can't fnd path"
