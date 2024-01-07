import Data.List (isInfixOf)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  promptString "Enter username: "
  user <- getLine
  validateUser user
  promptString "Enter password: "
  password <- getLine
  validatePassword password
  createUser user password dbs

promptString :: String -> IO ()
promptString msg = do
  putStr msg
  hFlush stdout

dbs :: [String]
dbs = ["kek", "kekus"]

validateUser :: String -> IO String
validateUser user
  | "postgres" `isInfixOf` user = do
      putStrLn "Username cannot contain postgres"
      exitFailure
  | otherwise = return user

validatePassword :: String -> IO String
validatePassword password
  | length password < 9 = do
      putStrLn "Password too short"
      exitFailure
  | otherwise = return password

createUser :: String -> String -> [String] -> IO ()
createUser user password dbs = do
  writeFile file_name $ printf "CREATE USER %s WITH PASSWORD '%s';\n" user password
  where
    -- mapM_ putStrLn $ map (\db -> db ++ ".sql") dbs

    file_name = printf "%s.sql" user