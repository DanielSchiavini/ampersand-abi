module GenErrorList where

import Text.Parsec
import Text.Parsec.Error
import Control.Applicative ((*>), (<*))
import System.IO

type CsvParser a = Parsec String () a

csvFile :: CsvParser [[String]]
csvFile = line `endBy` eol
    where eol = char '\n'

line = cell `sepBy` comma
    where comma = char ','

cell = quotedCell <|> simpleCell
    where quotedCell = char '\"' *> many (noneOf ['\"']) <* char '\"'
          simpleCell = many (noneOf ",\n")

parseCSV :: FilePath -> String -> Either ParseError [[String]]
parseCSV file input = parse csvFile file input

main :: IO ()
main = do let file = "ErrorList.csv"
          csv <- readFile file
          case parseCSV file csv of
                Left err  -> putStrLn (show (errorPos err) ++ ":" ++ showErr (errorMessages err))
                Right xss -> putStrLn ("result: " ++ show xss)
          where showErr = showErrorMessages "or" "unknown parse error"   "expecting" "unexpected" "end of input"