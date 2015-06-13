module GenErrorList where

import Text.Parsec
import Text.Parsec.Error
import Control.Applicative ((*>), (<*))
import Prelude hiding (readFile, writeFile, putStrLn)
import System.IO.UTF8
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
formatLatex (_:xss) = intercalate "\\\\\\hline\n" (map formatLine xss)
formatLatex _ = ""

formatLine :: [String] -> String
formatLine ("":_) = ""
formatLine (code:oldErr:oldEval:newErr:newEval:_) =
    intercalate " & " [ formatCode code
                      , formatError oldErr
                      , formatCell oldEval
                      , formatError newErr
                      , formatCell newEval
                      ]
    where formatCode code = "\\texttt{" ++ formatCell code ++ "}"
          formatError err = "\\texttt{" ++ formatCell err ++ "}"
formatLine _ = ""

formatCell :: String -> String
formatCell "" = ""
formatCell ('\xFFFD':cs)  = formatCell cs
formatCell ('&':cs)  = "\\&" ++ formatCell cs
formatCell ('_':cs)  = "\\_" ++ formatCell cs
formatCell ('#':cs)  = "\\#" ++ formatCell cs
formatCell ('{':cs)  = "\\{" ++ formatCell cs
formatCell ('}':cs)  = "\\}" ++ formatCell cs
formatCell ('\\':cs) = "\\textbackslash{}" ++ formatCell cs
formatCell ('\n':cs) = "\\newline\n  " ++ formatCell cs
formatCell (c:cs)    = (c:formatCell cs)

main :: IO ()
main = do let file = "ErrorList.csv"
          csv <- readFile file
          case parseCSV file csv of
                Left err  -> putStrLn (show (errorPos err) ++ ":" ++ showErr (errorMessages err))
                Right xss -> writeFile "GenErrorList.tex" (formatLatex xss)
          where showErr = showErrorMessages "or" "unknown parse error"   "expecting" "unexpected" "end of input"