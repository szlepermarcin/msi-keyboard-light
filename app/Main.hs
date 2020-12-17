module Main where


import           Control.Monad
import           System.Environment
import           System.Exit
import           MsiKeyboard

parse :: Maybe FilePath -> Maybe String -> [String] -> String -> IO ()
parse (Just config) preset [] _ = run config preset
parse _ preset ("-c" : config : xs) p = parse (Just config) preset xs p
parse config _ ("-p" : preset : xs) p = parse config (Just preset) xs p
parse _ _ ["-h"] p = putStrLn $ "usage: " ++ p ++ " -c file [-p] [preset] [-h]"
parse _ _ _ p =
  putStrLn ("usage: " ++ p ++ " -c file [-p] [preset] [-h]") >> exitFailure

main :: IO ()
main = join $ parse Nothing Nothing <$> getArgs <*> getProgName
