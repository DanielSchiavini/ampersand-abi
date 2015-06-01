module GenEbnf where

import Debug.Trace
import System.IO
import Data.Text(pack, unpack, splitOn)

cleanup :: String -> String
cleanup ('_':rest) = "\\_" ++ cleanup rest
cleanup ('#':rest) = "\\#" ++ cleanup rest
cleanup ('\\':rest) = "\\textbackslash{}" ++ cleanup rest
cleanup (x:rest) = (x : cleanup rest)
cleanup [] = []

printEbnfRule :: String -> String
printEbnfRule x =
    let (nm:pr:[]) = splitOn (pack " ::=") (pack x)
        name = unpack nm
        prod = cleanup (unpack pr)
    in unlines [ " \\begin{figure}[H]"
               , "  \\centering"
               , "  \\includegraphics[resolution=120,max size={\\textwidth}{\\textheight}]"
               , "  {Figures/Ebnf/" ++ name ++ "}"
               , "  \\caption*{\\texttt{" ++ name ++ " \\small::= " ++ prod ++ "}}"
               , "  \\label{fig:ebnf-" ++ name ++ "}"
               , " \\end{figure}"
               ]

main :: IO ()
main = do rules <- readFile "../../../ampersand-abi/Ebnf/ADL.ebnf"
          let tex = map printEbnfRule (lines rules)
          writeFile "GenEbnf.tex" (unlines tex)
          return ()