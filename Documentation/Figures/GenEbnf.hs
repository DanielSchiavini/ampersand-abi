module GenEbnf where

import Debug.Trace
import System.IO
import Data.Text(pack, unpack, splitOn)

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

printEbnfRule :: String -> String
printEbnfRule x =
    let (nm:pr:[]) = splitOn (pack " ::=") (pack x)
        name = unpack nm
        prod = cleanup (unpack pr)
    in unlines [ " \\begin{figure}[H]"
               , "  \\centering"
               , "  \\includegraphics[resolution=120,max size={\\textwidth}{\\textheight}]{Figures/Ebnf/" ++ name ++ "}"
               , "  \\caption*{\\texttt{" ++ name ++ " \\small::= " ++ prod ++ "}}"
               , "  \\label{fig:ebnf-" ++ name ++ "}"
               , " \\end{figure}"
               ]

main :: IO ()
main = do rules <- readFile "../../../ampersand-abi/Ebnf/ADL.ebnf"
          let tex = map printEbnfRule (lines rules)
          writeFile "GenEbnf.tex" (unlines tex)
          return ()