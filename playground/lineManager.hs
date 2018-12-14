-- A simple program to test file IO
import System.IO
import System.Environment
import System.Directory
import Data.List

main = do
      args <- getArgs
      let filename = (args!!0)
      handle <- openFile filename ReadMode
      putStrLn "File Contents:"
      contents <- hGetContents handle
      let fLines = lines contents
          numbered_lines = zipWith (\n line -> show n ++ " - " ++ line) [0..] fLines
      putStr $ unlines numbered_lines
      hClose handle
      putStrLn "\nWhat do you want to do?"
      putStrLn "1 - delete a line."
      putStrLn "2 - add a line to the file."
      putStrLn "other - nothing. Exit the program."
      line <- getLine
      let option = read line :: Int
      if (option == 1) || (option == 2)
      then do
           newContents <- handleOption option fLines
           (tempName, tempHandle) <- openTempFile "." "temp"
           hPutStr tempHandle newContents
           hClose tempHandle
           removeFile filename
           renameFile tempName filename
      else
          putStrLn "Ok then. Bye"

handleOption 1 cLines = do
                 putStrLn "What line you would like to delete?"
                 nStr <- getLine
                 let n = read nStr :: Int
                     newLines = delete (cLines !! n) cLines
                     contents = unlines newLines
                 return contents

handleOption 2 cLines = do
                putStrLn "Type your line"
                line <- getLine
                let contents = unlines $ line : cLines
                return contents
