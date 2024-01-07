import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  promptString "Enter username: "
  user <- getLine
  let user = map toLower user
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
dbs = []

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
  mapM_
    ( \db ->
        appendFile file_name $ printf "GRANT CONNECT ON DATABASE %s TO %s;\n" db user
    )
    dbs
  mapM_
    ( \db -> do
        appendFile file_name $ printf "\\c %s\n" db
        appendFile file_name $ printf "GRANT USAGE ON SCHEMA public TO %s;\n" user
        appendFile file_name $ printf "GRANT SELECT ON ALL TABLES IN SCHEMA public TO %s;\n" user
    )
    dbs
  where
    file_name = printf "%s.sql" user