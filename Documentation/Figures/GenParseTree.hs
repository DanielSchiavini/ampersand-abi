module GenParseTree where

import Debug.Trace
import System.IO
import Language.Haskell.Syntax
import Language.Haskell.Parser

data ParseNode = ParseNode String [Constructor] deriving (Show)
data Constructor = Constructor String [Record] deriving (Show)
data Record = Record String Mult deriving (Show)
data Mult = One | Optional | Many deriving (Show)

toName :: HsName -> String
toName (HsIdent nm)  = nm
toName (HsSymbol nm) = nm

toQName :: HsQName -> String
toQName (Qual (Module mod) nm) = mod ++ "." ++ toName nm
toQName (UnQual nm)            = toName nm	
toQName (Special special)      = show special

toRecord :: HsBangType -> Record
toRecord t = case t of HsBangedTy a   -> tRec a
                       HsUnBangedTy a -> tRec a
             where tRec :: HsType -> Record
                   -- tRec (HsTyFun t1 t2) = Record (show t1 ++ "*" ++ show t2) One
                   -- tRec (HsTyTuple ts)  = Record (show ts) One
                   -- tRec (HsTyApp t1 t2) = Record (show t1 ++ "*" ++ show t2) One
                   tRec (HsTyApp (HsTyCon (Special HsListCon)) other) = Record (nameOf other) Many
                   tRec (HsTyApp (HsTyCon (UnQual (HsIdent "Maybe"))) other) = Record (nameOf other) Optional
                   tRec (HsTyApp t1 _)  = Record (nameOf t1) One
                   tRec (HsTyVar nm)    = Record (toName nm) One
                   tRec (HsTyCon qnm)   = Record (toQName qnm) One
                   tRec ty = Record (show ty) One
                   nameOf :: HsType -> String
                   nameOf (HsTyCon nm) = toQName nm
                   nameOf (HsTyApp t1 _) = nameOf t1

toConstr :: HsConDecl -> Constructor
toConstr (HsConDecl _ nm types) = Constructor (toName nm) (map toRecord types)
toConstr (HsRecDecl _ nm record) = Constructor (toName nm) (map (toRecord.snd) record)

toNode :: HsDecl -> ParseNode
toNode (HsDataDecl _ _ name _ cs _) = ParseNode (toName name) (map toConstr cs)

dataTypes :: HsModule -> [ParseNode]
dataTypes (HsModule _ _ _ _ ds) = map toNode $ filter isDataDecl ds
    where isDataDecl :: HsDecl -> Bool
          isDataDecl (HsDataDecl _ _ _ _ _ _) = True
          isDataDecl _ = False

getDataTypes :: String -> [ParseNode]
getDataTypes mod = case parseModule mod of
                        ParseOk a -> dataTypes a
                        ParseFailed loc err -> trace (show loc ++ err) []

printNode :: ParseNode -> [String]
printNode (ParseNode nm css) =
    case css of (c:cs) -> printConstr nm c ++ printNode (ParseNode nm cs)
                []     -> [quote nm]
    where printConstr :: String -> Constructor -> [String]
          printConstr node (Constructor con rss) =
                case rss of (r:rs) -> printRec con r ++ printConstr node (Constructor con rs)
                            []     -> [arrow node con]
          printRec con (Record r m) = [arrow con r]
          arrow from to = quote from ++ "->" ++ quote to
          quote name = "[" ++ name ++ "]"

main :: IO ()
main = do mod <- readFile "../../../ampersand/src/Database/Design/Ampersand/Core/ParseTree.hs"
          let ds = getDataTypes mod
          writeFile "GenParseTree.txt" (unlines $ map show ds)
          writeFile "GenParseTree.yuml" (unlines.concat $ map printNode ds)
          return ()