import Control.Monad (when)
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
    when ("postgres" `isInfixOf` user_input) $ do
      putStrLn "Username cannot contain postgres"
      exitFailure
    pure user_input
  displayMessage "Enter password: "
  password <- do
    password_input <- getLine
    when (length password_input < 9) $ do
      putStrLn "Password too short"
      exitFailure
    pure password_input
  writeFile (user ++ ".sql") $
    createUserSQL user password inputsDB
  writeFile ("revoke_" ++ user ++ ".sql") $
    revokeUserSQL user inputsDB
  writeFile ("chpw_" ++ user ++ ".sql") $
    changePassword user password
  result <- verifyResult user $ user ++ ".sql"
  putStrLn result

displayMessage :: String -> IO ()
displayMessage message = do
  putStr message
  hFlush stdout

inputsDB :: [String]
inputsDB = []

inputsEnvVar :: [String]
inputsEnvVar = ["PGHOST", "PGPASSWORD"]

promptAction action = do
  undefined

verifyResult :: String -> FilePath -> IO String
verifyResult user file_name = do
  putStrLn ""
  content <- readFile file_name
  putStrLn content
  putStrLn "Is this OK? (y/n)"
  answer <- getLine
  case answer of
    "y" -> pure content
    "n" -> do
      removeFile file_name
      pure $ printf "Discarded .sql script for %s." user
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
  pure $ getMissingVars env_vars results

getMissingVars :: [String] -> [Maybe String] -> EnvVarUndefined
getMissingVars env_vars results =
  [env ++ " environment variable not defined" | (env, result) <- zip env_vars results, isNothing result]

createUserSQL :: String -> String -> [String] -> String
createUserSQL user passwd dbs =
  unlines $
    [createUserStatement]
      ++ map mkConnectGrant dbs
      ++ map mkUsageGrant dbs
  where
    createUserStatement = printf "CREATE USER %s WITH PASSWORD '%s';" user passwd

    mkConnectGrant db = printf "GRANT CONNECT ON DATABASE %s TO %s;" db user

    mkUsageGrant db =
      printf "\\c %s\n" db
        ++ "\n"
        ++ printf "GRANT USAGE ON SCHEMA public TO %s;" user
        ++ "\n"
        ++ printf "GRANT SELECT ON ALL TABLES IN SCHEMA public TO %s;" user

revokeUserSQL :: String -> [String] -> String
revokeUserSQL user dbs = do
  unlines $
    map revokeDbGrant dbs
      ++ [revokeSchemaPublic]
      ++ [dropUserStatement]
  where
    revokeDbGrant db = printf "REVOKE ALL ON DATABASE %s FROM %s;\n" db user
    revokeSchemaPublic = printf "REVOKE ALL ON SCHEMA public FROM %s;\n" user
    dropUserStatement = printf "DROP USER %s;\n" user

changePassword :: String -> String -> String
changePassword user passwd = do
  unlines [changePasswordStatement user passwd]
  where
    changePasswordStatement = printf "ALTER USER %s WITH PASSWORD '%s';"
