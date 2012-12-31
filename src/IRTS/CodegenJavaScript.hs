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

import Data.Char
import Data.List
import System.IO

type NamespaceName = String

codegenJavaScript
  :: [(Name, SDecl)]
  -> FilePath
  -> OutputType
  -> IO ()
codegenJavaScript definitions filename outputType = do
  let mods = groupBy modGrouper definitions
  let output = concatMap createModule mods ++ "\nMain.main()"
  writeFile filename output
  where modGrouper :: (Name, SDecl) -> (Name, SDecl) -> Bool
        modGrouper (a, _) (b, _) = translateNamespace a == translateNamespace b

createModule :: [(Name, SDecl)] -> String
createModule [] = ""
createModule definitions =
  let modname = translateNamespace $ (fst . head) definitions
      body = concatMap (translateDeclaration modname . snd) definitions in
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
         "})("
      ++ modname
      ++ " || ("
      ++ modname
      ++ " = {})"
      ++ ");"

translateNamespace :: Name -> String
translateNamespace (UN _)    = "__IDR__"
translateNamespace (NS _ ns) = intercalate "." ns
translateNamespace (MN _ _)  = "__IDR__"

translateName :: Name -> String
translateName (UN name)   = name
translateName (NS name _) = translateName name
translateName (MN i name) = "__idr_" ++ show i ++ "_" ++ name ++ "__"

translateConstant :: Const -> String
translateConstant (I i)   = show i
translateConstant (BI i)  = show i
translateConstant (Fl f)  = show f
translateConstant (Ch c)  = show c
translateConstant (Str s) = show s

translateFunParameter params =
  intercalate "," $ map translateVariableName vars
  where
    vars = map Loc [0..(length params)]

translateDeclaration :: NamespaceName -> SDecl -> String
translateDeclaration modname (SFun name params stackSize body) =
     modname
  ++ "."
  ++ translateName name
  ++ " = function("
  ++ translateFunParameter params
  ++ "){return "
  ++ translateExpression modname body
  ++ "};\n"

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
     translateNamespace name
  ++ "."
  ++ translateName name
  ++ "("
  ++ intercalate "," (map translateVariableName vars)
  ++ ")"

translateExpression _ (SOp op vars)
  | LBMinus     <- op
  , (lhs:rhs:_) <- vars = translateBinaryOp "-" lhs rhs
  | LBPlus      <- op
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

translateExpression modname (SChkCase var cases) =
     "(function(e){"
  ++ "switch(e){"
  ++ concatMap translateCases cases
  ++ "}})("
  ++ translateVariableName var
  ++ ")"
  where translateCases :: SAlt -> String
        translateCases (SDefaultCase e) =
             "default:return "
          ++ translateExpression modname e
          ++ ";break;"
        translateCases (SConstCase cst e) =
             translateConstant cst
          ++ ":return "
          ++ translateExpression modname e
          ++ ";break;"

translateExpression _ e = 
  "(function(){throw 'Not yet implemented: " ++ show e ++ "';})()"
