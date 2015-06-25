module GenEbnf where

import Control.Monad
import Data.List
import Data.String.Utils
import Data.Text (pack, unpack, Text, splitOn)
import System.Directory
import System.IO

cleanup :: String -> String
cleanup txt = clean txt False
  -- adds a second parameter to check if the quotes are start or end quotes
  where clean ('_':rest)  iq    = "\\_" ++ clean rest iq
        clean ('#':rest)  iq    = "\\#" ++ clean rest iq
        clean ('{':rest)  iq    = "\\{" ++ clean rest iq
        clean ('}':rest)  iq    = "\\}" ++ clean rest iq
        clean ('\\':rest) iq    = "\\textbackslash{}" ++ clean rest iq
        clean ('\'':rest) False = ('`' : clean rest True)
        clean ('\'':rest) True  = ('\'' : clean rest False)
        clean (x:rest)    iq    = (x : clean rest iq)
        clean []          _     = []

splitRule :: String -> (String, String)
splitRule x = f $ splitOn (pack " ::=") (pack x)
    where f :: [Text] -> (String, String)
          f (nm:pr:[]) = (unpack nm, cleanup (unpack pr))
          f _          = ("Invalid Rule", x)

printEbnfRule :: String -> String
printEbnfRule x =
    let (name,prod) = splitRule x
    in unlines [ " \\begin{figure}[H]"
               , "  \\centering"
               , "  \\includegraphics[resolution=120,max size={\\textwidth}{\\textheight}]{Figures/Ebnf/" ++ name ++ "}"
               , "  \\caption*{\\texttt{" ++ name ++ " \\small::= " ++ prod ++ "}}"
               , "  \\label{fig:ebnf-" ++ name ++ "}"
               , " \\end{figure}"
               ]

getFiles :: FilePath -> String -> IO [FilePath]
getFiles dir ext =
    do contents <- getDirectoryContents dir
       let valid = filter (endswith ext) contents
       let paths = map (dir ++) valid
       files <- filterM doesFileExist paths
       return $ sort files

extractRules :: String -> [String]
extractRules file =
    let trim  = map strip $ lines file
        filt = filter (startswith "--- ") trim
    in  map (drop 4) filt

getFileRules :: FilePath -> IO [String]
getFileRules fn = do file  <- readFile fn
                     return $ extractRules file

getRules :: IO [String]
getRules = do fs <- getFiles "../../../ampersand/src/Database/Design/Ampersand/Input/ADL1/" ".hs"
              rules <- mapM getFileRules fs
              return $ concat rules

main :: IO ()
main = do rules <- getRules
          let tex = map printEbnfRule rules
          writeFile "ADL.ebnf" (unlines rules)
          writeFile "GenEbnf.tex" (unlines tex)
          return ()
