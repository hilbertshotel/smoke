module Error where

unknown :: String -> IO ()
unknown sub = putStrLn ("error: unknown subcommand `" ++ sub ++ "`")

many :: IO ()
many = putStrLn "error: too many arguments"

missing :: String -> IO ()
missing path = putStrLn ("error: could not find " ++ path)

exists :: String -> IO ()
exists name = putStrLn ("error: `" ++ name ++ "` already exists")

new :: String -> IO ()
new = putStrLn ("error: new subcommand requires a name")

noargs :: String -> IO ()
noargs cmd = putStrLn ("subcommand " ++ cmd ++ " takes no arguments")
