import Control.Monad.RWS (MonadWriter (pass))
import Data.List (isInfixOf)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  promptString "Enter username: "
  user <- getLine
  promptString "Enter password: "
  password <- getLine
  createUser user password dbs

promptString :: String -> IO ()
promptString msg = do
  putStr msg
  hFlush stdout

dbs :: [String]
dbs = ["kek", "kekus"]

validateUser :: String -> Bool
validateUser "postgres" = False
validateUser user = not ("postgres" `isInfixOf` user)

validatePassword :: String -> Bool
validatePassword password = length password >= 9

createUser :: String -> String -> [String] -> IO ()
createUser user password dbs
  | not $ validateUser user = putStrLn "Username cannot contain postgres"
  | not $ validatePassword password = putStrLn "Password too short"
  | otherwise = do
      writeFile file_name $ printf "CREATE USER %s WITH PASSWORD '%s';\n" user password
  where
    -- mapM_ putStrLn $ map (\db -> db ++ ".sql") dbs

    file_name = printf "%s.sql" user