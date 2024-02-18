import Control.Monad (when)
import Control.Monad.Reader (ask, liftIO, runReaderT, ReaderT)
import Data.Char (toLower)
import Data.Foldable (traverse_)
import Data.List (isInfixOf, intercalate)
import Data.Maybe (isNothing)
import System.Directory (removeFile)
import System.Environment (lookupEnv)
import System.Exit (exitSuccess, die)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

newtype Username = User { getUser :: String }
newtype Password = Password { getPassword :: String }
newtype DatabaseList = DBList { getDBList :: [String] }
type SqlStatement = String

main :: IO ()
main = do
  envVars <- runReaderT parseEnvVars inputsEnvVar
  when (any (isNothing . snd) envVars) $ do
    let missingVars = fst <$> filter (isNothing . snd) envVars
    let errorMessage = unlines $ (++ " environment variable not defined") <$> missingVars
    die errorMessage
  displayMessage "Enter username: "
  user <- promptUser
  when ("postgres" `isInfixOf` getUser user) $
   die "Username cannot contain postgres"
  displayMessage "Enter password: "
  password <- promptPassword
  when (length (getPassword password) < 9) $
    die "Password too short"
  promptAction user password inputsDB

displayMessage :: String -> IO ()
displayMessage message = do
  putStr message
  hFlush stdout

inputsDB :: DatabaseList
inputsDB = DBList ["web", "web-api"]

inputsEnvVar :: [String]
inputsEnvVar = ["PGHOST", "PGPASSWORD"]

promptUser :: IO Username
promptUser = User <$> getLine

promptPassword :: IO Password
promptPassword = Password <$> getLine

promptAction :: Username -> Password -> DatabaseList -> IO ()
promptAction username password dbs = do
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
        userCreateSQL username password dbs
      result <- verifyResult username file_name
      print result
      where
        file_name = getUser username ++ ".sql"
    "2" -> do
      writeFile file_name $
        userRevokeSQL username dbs
      result <- verifyResult username file_name
      print result
      where
        file_name = "revoke_" ++ getUser username ++ ".sql"
    "3" -> do
      writeFile file_name $
        userPasswordChangeSQL username password
      result <- verifyResult username file_name
      print result
      where
        file_name = "chpw_" ++ getUser username ++ ".sql"
    "q" -> do
      putStrLn "Exiting...\n"
      exitSuccess
    _ -> do
      putStrLn "Unsupported input\n"
      promptAction username password dbs

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
      putStrLn $ "Discarded .sql script for " ++ getUser username
      pure False
    _ -> do
      putStrLn "Only y/n are accepted."
      verifyResult username file_name

parseEnvVars :: ReaderT [String] IO [(String, Maybe String)]
parseEnvVars = do
  envVars <- ask
  liftIO $
    traverse (
      \var -> do
        result <- lookupEnv var
        pure (var, result)
        ) envVars

userCreateSQL :: Username -> Password -> DatabaseList -> SqlStatement
userCreateSQL username password dbs =
  unlines $
    [createUserStatement]
      ++ (mkConnectGrant <$> getDBList dbs)
      ++ (mkUsageGrant <$> getDBList dbs)
  where
    createUserStatement = printf "CREATE USER %s WITH PASSWORD '%s';" (getUser username) (getPassword password)
    mkConnectGrant db = printf "GRANT CONNECT ON DATABASE %s TO %s;" db (getUser username)
    mkUsageGrant db =
      printf "\\c %s\n" db
        ++ "\n"
        ++ printf "GRANT USAGE ON SCHEMA public TO %s;" (getUser username)
        ++ "\n"
        ++ printf "GRANT SELECT ON ALL TABLES IN SCHEMA public TO %s;" (getUser username)

userRevokeSQL :: Username -> DatabaseList -> SqlStatement
userRevokeSQL username dbs = do
  unlines $
    mkRevokeGrant <$> getDBList dbs
      ++ [revokeSchemaPublic]
      ++ [dropUserStatement]
  where
    mkRevokeGrant db = printf "REVOKE ALL ON DATABASE %s FROM %s;\n" db (getUser username)
    revokeSchemaPublic = printf "REVOKE ALL ON SCHEMA public FROM %s;\n" (getUser username)
    dropUserStatement = printf "DROP USER %s;\n" (getUser username)

userPasswordChangeSQL :: Username -> Password -> SqlStatement
userPasswordChangeSQL username password = do
  unlines [userPasswordChangeSQLStatement (getUser username) (getPassword password)]
  where
    userPasswordChangeSQLStatement = printf "ALTER USER %s WITH PASSWORD '%s';"
