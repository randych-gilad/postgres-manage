import Control.Exception (SomeException, catch)
import Data.Char (toLower)
import Data.List (isInfixOf)
import System.Environment (getEnv, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, hPutStrLn, openFile, stderr, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  validateEnvVars
  displayMessage "Enter username: "
  user_input <- getLine
  let user = map toLower $ filter (/= ' ') user_input
  validateUser user
  displayMessage "Enter password: "
  password <- getLine
  validatePassword password
  createUser user password dbs
  revokeUser user dbs
  changePassword user password

displayMessage :: String -> IO ()
displayMessage message = do
  putStr message
  hFlush stdout

dbs :: [String]
dbs = []

validateEnvVars :: IO (Maybe (Maybe String), Maybe (Maybe String))
validateEnvVars = do
  pghost <- lookupEnv "PGHOST"
  pgpassword <- lookupEnv "PGPASSWORD"
  case (pghost, pgpassword) of
    (Nothing, Nothing) -> do
      putStrLn pghost_error
      putStrLn pgpassword_error
      exitFailure
    (Nothing, Just pgpassword) -> do
      putStrLn pghost_error
      exitFailure
    (Just pghost, Nothing) -> do
      putStrLn pgpassword_error
      exitFailure
    _ -> return (Just pghost, Just pgpassword)
  where
    pghost_error = "PGHOST environment variable is not defined"
    pgpassword_error = "PGPASSWORD environment variable is not defined"

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

revokeUser :: String -> [String] -> IO ()
revokeUser user dbs = do
  writeFile file_name ""
  mapM_
    ( \db ->
        appendFile file_name $ printf "REVOKE ALL ON DATABASE %s FROM %s;\n" db user
    )
    dbs
  appendFile file_name $ printf "REVOKE ALL ON SCHEMA public FROM %s;\n" user
  appendFile file_name $ printf "DROP USER %s;\n" user
  where
    file_name = printf "revoke_%s.sql" user

changePassword :: String -> String -> IO ()
changePassword user password = do
  writeFile file_name ""
  appendFile file_name $ printf "ALTER USER %s WITH PASSWORD '%s';" user password
  where
    file_name = printf "chpw_%s.sql" user