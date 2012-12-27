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

codegenJavaScript
  :: [(Name, SDecl)]
  -> FilePath
  -> OutputType
  -> IO ()
codegenJavaScript definitions filename outputType = do
  let body = concat $ map (translateDeclaration . snd) definitions
      output = concat [header "Main", body, footer]
  writeFile filename output


header :: String -> String
header modname =
  concatMap (++ "\n")
    ["var " ++ modname ++ " = (function(require) {",
     "var m = {};"]


footer :: String
footer =
  concatMap (++ "\n")
    ["return m;",
     "})();\n\nMain[\'Main.main\']();"]

translateName :: Name -> String
translateName = show

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

translateDeclaration :: SDecl -> String
translateDeclaration (SFun name params stackSize body) =
     "m[\'"
  ++ translateName name
  ++ "\'] = function("
  ++ translateFunParameter params
  ++ "){return "
  ++ translateExpression body
  ++ "}\n"

translateVariableName :: LVar -> String
translateVariableName (Loc i) =
  "__var_" ++ show i

translateExpression :: SExp -> String
translateExpression (SLet name value body) =
     "(function("
  ++ translateVariableName name
  ++ "){return "
  ++ translateExpression body
  ++ "})("
  ++ translateExpression value
  ++ ")"

translateExpression (SConst cst) =
  translateConstant cst

translateExpression (SV var) =
  translateVariableName var

translateExpression (SApp tc name vars) =
     "m[\'"
  ++ show name
  ++ "\']("
  ++ concatMap translateVariableName vars
  ++ ")"

translateExpression (SOp op vars)
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

translateExpression (SError msg) =
  "(function(){throw \'" ++ msg ++ "\';})();"

translateExpression (SForeign _ _ "putStr" [(FString, var)]) =
  "console.log(" ++ translateVariableName var ++ ");"

translateExpression (SChkCase var cases) =
     "(function(e){"
  ++ "switch(e){"
  ++ concatMap translateCases cases
  ++ "}})("
  ++ translateVariableName var
  ++ ")"
  where translateCases :: SAlt -> String
        translateCases (SDefaultCase e) =
             "default:return "
          ++ translateExpression e
          ++ ";break;"
        translateCases (SConstCase cst e) =
             translateConstant cst
          ++ ":return "
          ++ translateExpression e
          ++ ";break;"

translateExpression e = ""
--  '(' : show e ++ ")"
