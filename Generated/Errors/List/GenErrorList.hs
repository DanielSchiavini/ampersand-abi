module GenErrorList where

import Text.Parsec
import Text.Parsec.Error
import Control.Applicative ((*>), (<*))
import Prelude
import qualified System.IO.UTF8 as UTF
import Data.List

type CsvParser a = Parsec String () a

csvFile :: CsvParser [[String]]
csvFile = line `endBy` eol
    where eol = optional (char '\r') *> char '\n'

line :: CsvParser [String]
line = cell `sepBy` comma
    where comma = char ';'

cell :: CsvParser String
cell = quotedCell <|> simpleCell
    where quotedCell = quote *> many (doubleQuote <|> noneOf ['\"']) <* quote
          simpleCell = many (noneOf ";\n")
          doubleQuote = try (quote <* quote)
          quote = char '\"'

parseCSV :: FilePath -> String -> Either ParseError [[String]]
parseCSV file input = parse csvFile file input

formatLatex :: [[String]] -> String
formatLatex (_:xss) = intercalate "\n\n\\hrulefill\n\n" (map formatLine xss)
formatLatex _ = ""

formatLine :: [String] -> String
formatLine ("":_) = ""
formatLine (code:coveredOld:oldError:oldPosition:oldAccuracy:oldConcisenss:oldResult:newError:newPosition:newAccuracy:newConciseness:newResult:_) =
    intercalate "\n  " [ "\\begin{description}"
                       , "\\item[Incorrect ADL]" ++ formatCode code
                       , "\\item[Previous error]" ++ formatError oldError
                       , "\\item[Previous evaluation]" ++ formatEv oldPosition oldAccuracy oldConcisenss oldResult
                       , "\\item[New error]" ++ formatError newError
                       , "\\item[New evaluation]" ++ formatEv newPosition newAccuracy newConciseness newResult
                       , "\\end{description}"
                       ]
    where formatCode code = "~\\\\\n\\begin{adl}\n" ++ formatCell code ++ "\\end{adl}"
          formatError err = "~\\\\\n\\begin{haskell}\n" ++ formatCell err ++ "\\end{haskell}"
          formatEv pos acc conc res =
            intercalate "\n    " [ "~\\\\"
                                 , "\\begin{itemize}"
                                 , "\\item \\textbf{Position:} " ++ formatCell pos
                                 , "\\item \\textbf{Accuracy:} " ++ formatCell acc
                                 , "\\item \\textbf{Conciseness:} " ++ formatCell conc
                                 , "\\item \\textbf{Evaluation: " ++ formatCell res ++ "}"
                                 , "\\end{itemize}"
                                 ]
formatLine _ = ""

formatCell :: String -> String
formatCell "" = ""
--formatCell ('\xFFFD':cs)  = formatCell cs
--formatCell ('&':cs)  = "\\&" ++ formatCell cs
--formatCell ('_':cs)  = "\\_" ++ formatCell cs
--formatCell ('#':cs)  = "\\#" ++ formatCell cs
--formatCell ('{':cs)  = "\\{" ++ formatCell cs
--formatCell ('}':cs)  = "\\}" ++ formatCell cs
--formatCell ('\\':cs) = "\\textbackslash{}" ++ formatCell cs
--formatCell ('\n':cs) = "\\newline\n  " ++ formatCell cs
formatCell ('é':cs) = "e" ++ formatCell cs
formatCell (c:cs)    = (c:formatCell cs)

main :: IO ()
main = do let file = "ErrorList.csv"
          csv <- readFile file
          case parseCSV file csv of
                Left err  -> putStrLn (show (errorPos err) ++ ":" ++ showErr (errorMessages err))
                Right xss -> UTF.writeFile "GenErrorList.tex" (formatLatex xss)
          where showErr = showErrorMessages "or" "unknown parse error"   "expecting" "unexpected" "end of input"