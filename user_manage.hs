import Data.List (isInfixOf)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

main :: IO ()
main = do
  putStr "Enter username: "
  hFlush stdout
  user <- getLine
  print $ validateUser user
  print $ createUser

dbs :: [String]
dbs = []

validateUser :: String -> Bool
validateUser "postgres" = False
validateUser user = not ("postgres" `isInfixOf` user)

validatePassword :: String -> Bool
validatePassword password = length password < 9

createUser :: String -> String -> [String] -> IO ()
createUser user password db
  | not validateUser user = ()
  | not validatePassword password = ()
  | otherwise = do
      mapM_ putStrLn $ map (\db -> db ++ ".sql") dbs