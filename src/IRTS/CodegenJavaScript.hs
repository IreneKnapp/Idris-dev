{-# LANGUAGE PatternGuards #-}

module IRTS.CodegenJavaScript (codegenJavaScript) where

import Idris.AbsSyntax
import IRTS.Bytecode
import IRTS.Lang
import IRTS.Simplified
import IRTS.CodegenCommon
import Core.TT
import Paths_idris
import Util.System

import Control.Arrow
import Data.Char
import Data.List
import System.IO

type NamespaceName = String

idrNamespace :: NamespaceName
idrNamespace = "__IDR__"

codegenJavaScript
  :: [(Name, SDecl)]
  -> FilePath
  -> OutputType
  -> IO ()
codegenJavaScript definitions filename outputType = do
  print $ map snd definitions
  print $ map (translateNamespace . fst) definitions
  let def = map (first translateNamespace) definitions
  let output = idrRuntime ++ concatMap (translateModule Nothing) def ++ "\nMain.main()"
  writeFile filename output

idrRuntime :: String
idrRuntime =
  createModule Nothing idrNamespace $ concat
    [ "__IDR__.IntType = { type: 'IntType' };"
    , "__IDR__.Con = function(i,name,vars)"
    , "{this.i = i;this.name = name;this.vars =  vars;};"
    ]

createModule :: Maybe String -> NamespaceName -> String -> String
createModule toplevel modname body =
  concat [header modname, body, footer modname]
  where
    header :: NamespaceName -> String
    header modname =
      concatMap (++ "\n")
        [ "var " ++ modname ++ ";"
        , "(function(" ++ modname ++ "){"
        ]

    footer :: NamespaceName -> String
    footer modname =
      let m = maybe "" (++ ".") toplevel ++ modname in
         "\n})("
      ++ m
      ++ " || ("
      ++ m
      ++ " = {})"
      ++ ");\n\n"

translateModule :: Maybe String -> ([String], SDecl) -> String
translateModule toplevel ([modname], decl) =
  let body = translateDeclaration modname decl in
      createModule toplevel modname body
translateModule toplevel (n:ns, decl) =
  createModule toplevel n $ translateModule (Just n) (ns, decl)

translateIdentifier :: String -> String
translateIdentifier =
  concatMap replaceBadChars
  where replaceBadChars :: Char -> String
        replaceBadChars ' '  = "_"
        replaceBadChars '@'  = "__at__"
        replaceBadChars '['  = "__OSB__"
        replaceBadChars ']'  = "__CSB__"
        replaceBadChars '{'  = "__OB__"
        replaceBadChars '}'  = "__CB__"
        replaceBadChars '!'  = "__bang__"
        replaceBadChars '#'  = "__hash__"
        replaceBadChars '.'  = "__dot__"
        replaceBadChars ':'  = "__colon__"
        replaceBadChars '\'' = "__apo__"
        replaceBadChars c
          | isDigit c = "__" ++ [c] ++ "__"
          | otherwise = [c]

translateNamespace :: Name -> [String]
translateNamespace (UN _)    = [idrNamespace]
translateNamespace (NS _ ns) = map translateIdentifier ns
translateNamespace (MN _ _)  = [idrNamespace]

translateName :: Name -> String
translateName (UN name)   = translateIdentifier name
translateName (NS name _) = translateName name
translateName (MN i name) = translateIdentifier name ++ show i

translateQualifiedName :: Name -> String
translateQualifiedName name =
  intercalate "." (translateNamespace name) ++ "." ++ translateName name

translateConstant :: Const -> String
translateConstant (I i)   = show i
translateConstant (BI i)  = show i
translateConstant (Fl f)  = show f
translateConstant (Ch c)  = show c
translateConstant (Str s) = show s
translateConstant IType   = "__IDR__.IntType"
translateConstant c       =
  "(function(){throw 'Unimplemented Const: " ++ show c ++ "';})()"

translateParameterlist params =
  intercalate "," $ map translateVariableName vars
  where
    vars = map (\(MN i _) -> Loc i) params

translateDeclaration :: NamespaceName -> SDecl -> String
translateDeclaration modname (SFun name params stackSize body) =
     modname
  ++ "."
  ++ translateName name
  ++ " = function("
  ++ translateParameterlist params
  ++ "){"
  ++ concatMap allocVar [numP..(numP+stackSize)]
  ++ "return "
  ++ translateExpression modname body
  ++ ";};\n"
  where 
    numP :: Int
    numP = length params

    allocVar :: Int -> String
    allocVar n = "var __var_" ++ show n ++ ";"

translateVariableName :: LVar -> String
translateVariableName (Loc i) =
  "__var_" ++ show i

translateExpression :: NamespaceName -> SExp -> String
translateExpression modname (SLet name value body) =
     "(function("
  ++ translateVariableName name
  ++ "){return "
  ++ translateExpression modname body
  ++ "})("
  ++ translateExpression modname value
  ++ ")"

translateExpression _ (SConst cst) =
  translateConstant cst

translateExpression _ (SV var) =
  translateVariableName var

translateExpression modname (SApp tc name vars) =
     concat (intersperse "." $ translateNamespace name)
  ++ "."
  ++ translateName name
  ++ "("
  ++ intercalate "," (map translateVariableName vars)
  ++ ")"

translateExpression _ (SOp op vars)
  | LPlus       <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "+" lhs rhs
  | LMinus      <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "-" lhs rhs
  | LTimes      <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "*" lhs rhs
  | LDiv        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "/" lhs rhs
  | LMod        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "%" lhs rhs
  | LEq         <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "===" lhs rhs
  | LLt         <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "<" lhs rhs
  | LLe         <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "<=" lhs rhs
  | LGt         <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp ">" lhs rhs
  | LGe         <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp ">=" lhs rhs

  | LFPlus      <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "+" lhs rhs
  | LFMinus     <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "-" lhs rhs
  | LFTimes     <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "*" lhs rhs
  | LFDiv       <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "/" lhs rhs
  | LFEq        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "===" lhs rhs
  | LFLt        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "<" lhs rhs
  | LFLe        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "<=" lhs rhs
  | LFGt        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp ">" lhs rhs
  | LFGe        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp ">=" lhs rhs

  | LBPlus      <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "+" lhs rhs
  | LBMinus     <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "-" lhs rhs
  | LBTimes     <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "*" lhs rhs
  | LBDiv       <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "/" lhs rhs
  | LBMod       <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "%" lhs rhs
  | LBEq        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "===" lhs rhs
  | LBLt        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "<" lhs rhs
  | LBLe        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "<=" lhs rhs
  | LBGt        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp ">" lhs rhs
  | LBGe        <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp ">=" lhs rhs

  | LStrConcat  <-op
  , (lhs:rhs:_) <- vars = translateBinaryOp "+" lhs rhs
  
  where
    translateBinaryOp :: String -> LVar -> LVar -> String
    translateBinaryOp f lhs rhs =
         translateVariableName lhs
      ++ f
      ++ translateVariableName rhs

translateExpression _ (SError msg) =
  "(function(){throw \'" ++ msg ++ "\';})();"

translateExpression _ (SForeign _ _ "putStr" [(FString, var)]) =
  "console.log(" ++ translateVariableName var ++ ");"

translateExpression _ (SForeign _ _ fun args) =
     fun
  ++ "("
  ++ intercalate "," (map (translateVariableName . snd) args)
  ++ ");"

translateExpression modname (SChkCase var cases) =
     "(function(e){"
  ++ intercalate " else " (map (translateCase modname "e") cases)
  ++ "})("
  ++ translateVariableName var
  ++ ")"

translateExpression modname (SCase var cases) = 
     "(function(e){"
  ++ intercalate " else " (map (translateCase modname "e") cases)
  ++ "})("
  ++ translateVariableName var
  ++ ")"

translateExpression _ (SCon i name vars) =
  concat [ "new __IDR__.Con("
         , show i
         , ","
         , '\'' : translateQualifiedName name ++ "\',["
         , intercalate "," $ map translateVariableName vars
         , "])"
         ]

translateExpression modname (SUpdate var e) =
     "(function(){return ("
  ++ translateVariableName var
  ++ " = " ++ translateExpression modname e
  ++ ");})()"

translateExpression modname (SProj var i) =
     "(function(){return "
  ++ translateVariableName var ++ ".vars[" ++ show i ++"];})()"

translateExpression _ SNothing = "null"

translateExpression _ e =
     "(function(){throw 'Not yet implemented: "
  ++ filter (/= '\'') (show e)
  ++ "';})()"

translateCase :: String -> String -> SAlt -> String
translateCase modname _ (SDefaultCase e) =
  createIfBlock "true" (translateExpression modname e)

translateCase modname var (SConstCase cst e) =
  let cond = var ++ " === " ++ translateConstant cst in
      createIfBlock cond (translateExpression modname e)

translateCase modname var (SConCase _ i name vars e) =
  let isCon = var ++ " instanceof __IDR__.Con"
      isI = show i ++ " === " ++ var ++ ".i"
      params = translateParameterlist vars
      args = ".apply(this," ++ var ++ ".vars)"
      f b =
           "(function("
        ++ params 
        ++ "){return " ++ b ++ "})" ++ args
      cond = intercalate " && " [isCon, isI] in
      createIfBlock cond $ f (translateExpression modname e)

createIfBlock cond e =
     "if (" ++ cond ++") {"
  ++ "return " ++ e
  ++ ";}"
