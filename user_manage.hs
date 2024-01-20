import Control.Monad (when)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

type EnvVars = [String]

type EnvVarUndefined = [String]

type Username = String

type Password = String

type DBs = [String]

type SqlStatement = String

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

promptAction :: Username -> Password -> DBs -> IO ()
promptAction username passwd dbs = do
  putStrLn "Choose action:"
  putStrLn "[1] Create username"
  putStrLn "[2] Revoke username"
  putStrLn "[3] Change password"
  putStrLn "[q] Exit\n"
  putStr "Selected option: "
  action <- getLine
  case action of
    "1" -> do
      writeFile file_name $
        createUserSQL username passwd dbs
      result <- verifyResult username file_name
      print result
      where
        file_name = username ++ ".sql"
    "2" -> do
      writeFile file_name $
        revokeUserSQL username dbs
      result <- verifyResult username file_name
      print result
      where
        file_name = "revoke_" ++ username ++ ".sql"
    "3" -> do
      writeFile file_name $
        changePassword username passwd
      result <- verifyResult username file_name
      print result
      where
        file_name = "chpw_" ++ username ++ ".sql"
    "q" -> do
      putStrLn "Exiting...\n"
      exitSuccess
    _ -> do
      putStrLn "Unsupported input\n"
      promptAction username passwd dbs

verifyResult :: Username -> FilePath -> IO Bool
verifyResult username file_name = do
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
      putStrLn $ "Discarded .sql script for " ++ username
      pure False
    _ -> do
      putStrLn "Only y/n are accepted."
      verifyResult username file_name

reportMissingVars :: EnvVars -> IO ()
reportMissingVars envVars = do
  toValidate <- lookupMissingVars envVars
  case toValidate of
    [] -> putStr ""
    _ -> do
      mapM_ putStrLn toValidate
      exitFailure

lookupMissingVars :: EnvVars -> IO [String]
lookupMissingVars env_vars = do
  results <- mapM lookupEnv env_vars
  pure $ getMissingVars env_vars results

getMissingVars :: EnvVars -> [Maybe String] -> EnvVarUndefined
getMissingVars env_vars results =
  [env ++ " environment variable not defined" | (env, result) <- zip env_vars results, isNothing result]

createUserSQL :: Username -> Password -> DBs -> SqlStatement
createUserSQL username passwd dbs =
  unlines $
    [createUserStatement]
      ++ map mkConnectGrant dbs
      ++ map mkUsageGrant dbs
  where
    createUserStatement = printf "CREATE USER %s WITH PASSWORD '%s';" username passwd

    mkConnectGrant db = printf "GRANT CONNECT ON DATABASE %s TO %s;" db username

    mkUsageGrant db =
      printf "\\c %s\n" db
        ++ "\n"
        ++ printf "GRANT USAGE ON SCHEMA public TO %s;" username
        ++ "\n"
        ++ printf "GRANT SELECT ON ALL TABLES IN SCHEMA public TO %s;" username

revokeUserSQL :: Username -> DBs -> SqlStatement
revokeUserSQL username dbs = do
  unlines $
    map revokeDbGrant dbs
      ++ [revokeSchemaPublic]
      ++ [dropUserStatement]
  where
    revokeDbGrant db = printf "REVOKE ALL ON DATABASE %s FROM %s;\n" db username
    revokeSchemaPublic = printf "REVOKE ALL ON SCHEMA public FROM %s;\n" username
    dropUserStatement = printf "DROP USER %s;\n" username

changePassword :: Username -> Password -> SqlStatement
changePassword username passwd = do
  unlines [changePasswordStatement username passwd]
  where
    changePasswordStatement = printf "ALTER USER %s WITH PASSWORD '%s';"
