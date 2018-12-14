-- A simple program to test file IO - version 2 -- more flexible
import System.IO
import System.Environment
import System.Directory
import Data.List

dispatch :: [(String, [String] -> IO())]
dispatch = [("add", add)
           ,("remove", remove)
           ,("view", view)
           ,("bump", bump)
           ]

main = do
    cmd:args <- getArgs
    let Just action = lookup cmd dispatch
    action args


add :: [String] -> IO()
add [filename, entry] =
    appendFile filename (entry ++ "\n")

remove :: [String] -> IO()
remove [filename, lineNumber] = do
    contents <- readFile filename
    let n = read lineNumber :: Int
        contentsLines = lines contents
        newContents = unlines (delete (contentsLines!!n) contentsLines)
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newContents
    hClose tempHandle
    removeFile filename
    renameFile tempName filename

view :: [String] -> IO()
view [filename] = do
    contents <- readFile filename
    let contentsLines = lines contents
        numberedLines = zipWith (\n line -> show n ++ " - " ++ line ) [0..] contentsLines
        numberedContents = unlines numberedLines
    putStr numberedContents

bump :: [String] -> IO()
bump [filename, lineNumber] = do
    contents <- readFile filename
    let n = read lineNumber :: Int
        contentsLines = lines contents
        choosenContent = contentsLines!!n
        tmpContentsL = delete choosenContent contentsLines
        newContents = unlines $ choosenContent : tmpContentsL
    (tempName, tempHandle) <- openTempFile "." "temp"
    hPutStr tempHandle newContents
    hClose tempHandle
    removeFile filename
    renameFile tempName filename
