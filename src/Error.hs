module Error where

unknown :: String -> IO ()
unknown sub = putStrLn ("error: unknown subcommand `" ++ sub ++ "`")

many :: IO ()
many = putStrLn "error: too many arguments"

missing :: String -> IO ()
missing path = putStrLn ("error: could not find " ++ path)

exists :: String -> IO ()
exists name = putStrLn ("error: `" ++ name ++ "` already exists")
