module Error where

unknown :: String -> IO ()
unknown sub = do
    let error = "error: unknown subcommand `" ++ sub ++ "`"
    putStrLn error

many :: IO ()
many = putStrLn "error: too many arguments"

missing :: String -> IO ()
missing path = do
    let error = "error: could not find " ++ path
    putStrLn error

exists :: String -> IO ()
exists name = do
    let error = "error: `" ++ name ++ "` already exists"
    putStrLn error
