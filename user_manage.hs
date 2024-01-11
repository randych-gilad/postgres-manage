import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

type EnvVarUndefined = [String]

main :: IO ()
main = do
  reportMissingVars inputsEnvVar
  displayMessage "Enter username: "
  user <- do
    user_input <- getLine
    validateUser user_input
  displayMessage "Enter password: "
  password <- do
    password_input <- getLine
    validatePassword password_input
  createUser user password inputsDB
  revokeUser user inputsDB
  changePassword user password
  result <- verifyResult user "testuser.sql"
  putStrLn result

displayMessage :: String -> IO ()
displayMessage message = do
  putStr message
  hFlush stdout

inputsDB :: [String]
inputsDB = []

inputsEnvVar :: [String]
inputsEnvVar = ["PGHOST", "PGPASSWORD"]

verifyResult :: String -> FilePath -> IO String
verifyResult user file_name = do
  putStrLn ""
  content <- readFile file_name
  putStrLn content
  putStrLn "Is this OK? (y/n)"
  answer <- getLine
  case answer of
    "y" -> return content
    "n" -> do
      removeFile file_name
      return (printf "Discarded .sql script for %s." user)
    _ -> do
      putStrLn "Only y/n are accepted."
      verifyResult user file_name

reportMissingVars :: [String] -> IO ()
reportMissingVars envVars = do
  toValidate <- lookupMissingVars envVars
  case toValidate of
    [] -> putStr ""
    _ -> do
      mapM_ putStrLn toValidate
      exitFailure

lookupMissingVars :: [String] -> IO [String]
lookupMissingVars env_vars = do
  results <- mapM lookupEnv env_vars
  return $ getMissingVars env_vars results

getMissingVars :: [String] -> [Maybe String] -> EnvVarUndefined
getMissingVars env_vars results =
  [env ++ " environment variable not defined" | (env, result) <- zip env_vars results, isNothing result]

validateUser :: String -> IO String
validateUser user
  | "postgres" `isInfixOf` user = do
      putStrLn "Username cannot contain postgres"
      exitFailure
  | otherwise = return $ map toLower $ filter (/= ' ') user

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
  putStrLn ("Created .sql script for " ++ user)
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