module Error where

unknown :: String -> IO ()
unknown sub = putStrLn ("error: unknown subcommand `" ++ sub ++ "`")

many :: IO ()
many = putStrLn "error: too many arguments"

missing :: String -> IO ()
missing path = putStrLn ("error: could not find `" ++ path ++ "`")

exists :: String -> IO ()
exists name = putStrLn ("error: `" ++ name ++ "` already exists")

noname :: String -> IO ()
noname cmd = putStrLn ("error: subcommand `" ++ cmd ++ "` requires a <name> argument")

noargs :: String -> IO ()
noargs cmd = putStrLn ("subcommand `" ++ cmd ++ "` takes no arguments")

git :: IO ()
git = putStrLn "error: failed to initialize `git`"

ghc :: IO ()
ghc = putStrLn "error: `ghc` is not installed on the system"

execPath :: IO ()
execPath = putStrLn "error: can't fnd path"