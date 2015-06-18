module GenErrorList (main) where

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
formatLatex (_:xss) = intercalate "\n\n\\hrulefill\n\n" $ map formatLine (zip [1..] xss)
formatLatex _ = ""

formatLine :: (Int, [String]) -> String
formatLine (i,("":_)) = ""
formatLine (i,(code:coveredOld:oldError:oldPosition:oldAccuracy:oldConcisenss:oldResult:newError:newPosition:newAccuracy:newConciseness:newResult:_)) =
    intercalate "\n  " [ "\\subsection{Error type " ++ formatNr i ++ "}"
                       , "\\begin{description}"
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
                                 , "\\item \\textbf{Accurate:} " ++ formatCell pos
                                 , "\\item \\textbf{Intuitive:} " ++ formatCell acc
                                 , "\\item \\textbf{Succint:} " ++ formatCell conc
                                 , "\\item \\textbf{Evaluation: " ++ formatCell res ++ "}"
                                 , "\\end{itemize}"
                                 ]
formatLine _ = ""

formatNr :: Int -> String
formatNr i | i < 10 = "0"++show i
           | otherwise = show i

writeIncorrectLaTeX :: [[String]] -> IO ()
writeIncorrectLaTeX xss = UTF.writeFile "GenErrorList.tex" (formatLatex xss)

writeIncorrectADLs :: [[String]] -> IO ()
writeIncorrectADLs (_:xss) = do mapM writeADL (zip [1..] xss); return ()
    where writeADL :: (Int, [String]) -> IO ()
          writeADL (i,(x:xs)) = UTF.writeFile ("../Scripts/ErrorType" ++ formatNr i ++ ".adl") x
writeIncorrectADLs _ = return ()

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
          csv <- UTF.readFile file
          case parseCSV file csv of
                Left err  -> putStrLn (show (errorPos err) ++ ":" ++ showErr (errorMessages err))
                Right xss -> do writeIncorrectLaTeX xss
                                writeIncorrectADLs xss
          where showErr = showErrorMessages "or" "unknown parse error"   "expecting" "unexpected" "end of input"
