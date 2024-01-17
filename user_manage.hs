import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
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
  promptAction user password inputsDB

displayMessage :: String -> IO ()
displayMessage message = do
  putStr message
  hFlush stdout

inputsDB :: [String]
inputsDB = []

inputsEnvVar :: [String]
inputsEnvVar = ["PGHOST", "PGPASSWORD"]

promptAction :: String -> String -> [String] -> IO ()
promptAction user password dbs = do
  putStrLn "Choose action:"
  putStrLn "[1] Create user"
  putStrLn "[2] Revoke user"
  putStrLn "[3] Change password"
  putStrLn "[q] Exit\n"
  putStr "Selected option: "
  action <- getLine
  case action of
    "1" -> do
      writeFile file_name $
        createUserSQL user password dbs
      result <- verifyResult user file_name
      print result
      where
        file_name = user ++ ".sql"
    "2" -> do
      writeFile file_name $
        revokeUserSQL user dbs
      result <- verifyResult user file_name
      print result
      where
        file_name = "revoke_" ++ user ++ ".sql"
    "3" -> do
      writeFile file_name $
        changePassword user password
      result <- verifyResult user file_name
      print result
      where
        file_name = "chpw_" ++ user ++ ".sql"
    "q" -> do
      putStrLn "Exiting...\n"
      exitSuccess
    _ -> do
      putStrLn "Unsupported input\n"
      promptAction user password dbs

verifyResult :: String -> FilePath -> IO Bool
verifyResult user file_name = do
  putStrLn ""
  content <- readFile file_name
  putStrLn content
  putStrLn "Is this OK? (y/n)"
  answer <- getLine
  case answer of
    "y" -> do
      undefined
      pure True -- should proceed to applyResult here but no idea how
      -- I don't feel like supplying more arguments just for sake of this
      -- while it should be own FilePath -> IO () function
    "n" -> do
      removeFile file_name
      putStrLn $ "Discarded .sql script for " ++ user
      pure False
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
