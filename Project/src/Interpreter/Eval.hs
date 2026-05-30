{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Interpreter.Eval where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Exception (throwIO)
import Control.Monad (forM)
import Parser.AST
import Interpreter.Value
import Lexer.Tokens (Flag(..))

-- | Evalúa una expresión dado un entorno
eval :: Env -> Expr -> IO Value
eval _   (EInt    n)            = return (VInt n)
eval _   (EFloat  f)            = return (VFloat f)
eval _   (EString s)            = return (VString s)
eval _   (EChar   c)            = return (VString (T.singleton c))
eval _   (EBool   b)            = return (VBool b)
eval _   EUnit                  = return VUnit
eval env (EVar name)            =
    case lookupVar name env of
        Right v  -> return v
        Left err -> throwIO err
eval _   (ECon name)            = return (VCon name [])
eval env (EApp func arg)        = do
    fv <- eval env func
    av <- eval env arg
    applyValue fv av
eval env (ELam args body)       = return $ curryLam env args body
eval env (ELet bindings body)   = do
    env' <- foldBindings env bindings
    eval env' body
eval env (EWhere body bindings) = do
    env' <- foldBindings env bindings
    eval env' body
eval env (EIf cond true false)  = do
    cv <- eval env cond
    case cv of
        VBool True  -> eval env true
        VBool False -> eval env false
        other -> throwIO $ TypeMismatch $
            "if esperaba Bool, obtuvo: " <> T.pack (show other)
eval env (ECase scrutinee alts) = do
    sv <- eval env scrutinee
    matchAlts env sv alts
eval env (EDo stmts)            = evalDo env stmts
eval env (ERef expr)            = do
    v   <- eval env expr
    ref <- newIORef v
    return (VRef ref)
eval env (EBinOp op left right) = do
    lv <- eval env left
    rv <- eval env right
    evalBinOp op lv rv
eval env (EUnOp op expr)        = do
    v <- eval env expr
    evalUnOp op v
eval env (EList exprs)          = VList  <$> mapM (eval env) exprs
eval env (ETuple exprs)         = VTuple <$> mapM (eval env) exprs

-- | Aplica un valor función a un argumento
applyValue :: Value -> Value -> IO Value
applyValue (VFun f)           arg = f arg
applyValue (VCon name fields) arg = return (VCon name (fields ++ [arg]))
applyValue other              _   = throwIO $ TypeMismatch $
    "se esperaba una función, se obtuvo: " <> T.pack (show other)

-- | Convierte lambda de múltiples args en funciones anidadas (currying)
curryLam :: Env -> [Text] -> Expr -> Value
curryLam env []     body = VFun $ \_ -> eval env body
curryLam env [x]    body = VFun $ \v -> eval (extendEnv x v env) body
curryLam env (x:xs) body = VFun $ \v ->
    return $ curryLam (extendEnv x v env) xs body

-- | Agrega bindings al entorno secuencialmente
foldBindings :: Env -> [(Text, Expr)] -> IO Env
foldBindings env [] = return env
foldBindings env ((name, expr) : rest) = do
    v    <- eval env expr
    foldBindings (extendEnv name v env) rest

-- | Intenta cada alternativa de case hasta que una matchea
matchAlts :: Env -> Value -> [CaseAlt] -> IO Value
matchAlts _   val [] = throwIO $ PatternFail $
    "ningún patrón matcheó para: " <> T.pack (show val)
matchAlts env val (CaseAlt pat body : rest) =
    case matchPat pat val of
        Nothing       -> matchAlts env val rest
        Just bindings -> eval (extendEnvMany bindings env) body

-- | Intenta matchear un patrón contra un valor
matchPat :: Pat -> Value -> Maybe [(Text, Value)]
matchPat PWild               _                = Just []
matchPat (PVar name)         val              = Just [(name, val)]
matchPat (PLit lit)          val              =
    case (lit, val) of
        (LInt    n, VInt    m) | n == m -> Just []
        (LFloat  f, VFloat  g) | f == g -> Just []
        (LString s, VString t) | s == t -> Just []
        (LBool   b, VBool   c) | b == c -> Just []
        _                               -> Nothing
matchPat (PCon name pats)    (VCon vname vals)
    | name == vname && length pats == length vals =
        fmap concat $ sequence $ zipWith matchPat pats vals
    | otherwise = Nothing
matchPat (PTuple pats)       (VTuple vals)
    | length pats == length vals =
        fmap concat $ sequence $ zipWith matchPat pats vals
    | otherwise = Nothing
matchPat (PList pats)        (VList vals)
    | length pats == length vals =
        fmap concat $ sequence $ zipWith matchPat pats vals
    | otherwise = Nothing
matchPat _                   _                = Nothing

-- | Ejecuta do notation
evalDo :: Env -> [DoStmt] -> IO Value
evalDo env []             = return VUnit
evalDo env [DoExpr e]     = eval env e
evalDo env (stmt : rest)  =
    case stmt of
        DoBind name expr -> do
            v <- eval env expr >>= unwrapIO
            evalDo (extendEnv name v env) rest
        DoExpr expr -> do
            _ <- eval env expr >>= unwrapIO
            evalDo env rest
        DoLet name expr -> do
            v <- eval env expr
            evalDo (extendEnv name v env) rest

-- | Desenvuelve un VIO ejecutando la acción
unwrapIO :: Value -> IO Value
unwrapIO (VIO action) = action
unwrapIO v            = return v

-- | Evalúa un operador binario
evalBinOp :: BinOp -> Value -> Value -> IO Value
evalBinOp OpAdd    (VInt   a) (VInt   b) = return $ VInt   (a + b)
evalBinOp OpAdd    (VFloat a) (VFloat b) = return $ VFloat (a + b)
evalBinOp OpSub    (VInt   a) (VInt   b) = return $ VInt   (a - b)
evalBinOp OpSub    (VFloat a) (VFloat b) = return $ VFloat (a - b)
evalBinOp OpMul    (VInt   a) (VInt   b) = return $ VInt   (a * b)
evalBinOp OpMul    (VFloat a) (VFloat b) = return $ VFloat (a * b)
evalBinOp OpDiv    (VInt   _) (VInt   0) = throwIO DivByZero
evalBinOp OpDiv    (VInt   a) (VInt   b) = return $ VInt   (a `div` b)
evalBinOp OpDiv    (VFloat a) (VFloat b) = return $ VFloat (a / b)
evalBinOp OpMod    (VInt   a) (VInt   b) = return $ VInt   (a `mod` b)
evalBinOp OpPow    (VInt   a) (VInt   b) = return $ VInt   (a ^ b)
evalBinOp OpEq     a          b          = return $ VBool  (show a == show b)
evalBinOp OpNeq    a          b          = return $ VBool  (show a /= show b)
evalBinOp OpLt     (VInt   a) (VInt   b) = return $ VBool  (a < b)
evalBinOp OpGt     (VInt   a) (VInt   b) = return $ VBool  (a > b)
evalBinOp OpLte    (VInt   a) (VInt   b) = return $ VBool  (a <= b)
evalBinOp OpGte    (VInt   a) (VInt   b) = return $ VBool  (a >= b)
evalBinOp OpAnd    (VBool  a) (VBool  b) = return $ VBool  (a && b)
evalBinOp OpOr     (VBool  a) (VBool  b) = return $ VBool  (a || b)
evalBinOp OpConcat (VString a) (VString b) = return $ VString (a <> b)
evalBinOp OpConcat (VList   a) (VList   b) = return $ VList   (a ++ b)
evalBinOp OpCons   v           (VList  vs) = return $ VList   (v : vs)
evalBinOp OpApply  (VFun f)    arg         = f arg
evalBinOp OpApply  _           _           = throwIO $ TypeMismatch "$ espera función a la izquierda"
evalBinOp OpComp   (VFun f)    (VFun g)    = return $ VFun $ \x -> g x >>= f
evalBinOp OpComp   _           _           = throwIO $ TypeMismatch ". espera dos funciones"
evalBinOp OpBind   (VIO action) (VFun f)   = return $ VIO $ do
    v <- action
    action' <- f v
    unwrapIO action'
evalBinOp OpBind   _           _           = throwIO $ TypeMismatch ">>= espera IO a la izquierda"
evalBinOp OpThen   (VIO a1)    (VIO a2)    = return $ VIO (a1 >> a2)
evalBinOp OpThen   _           _           = throwIO $ TypeMismatch ">> espera dos IO"
evalBinOp op       lv          rv          = throwIO $ TypeMismatch $
    "tipos incompatibles para operador: "
    <> T.pack (show lv) <> " y " <> T.pack (show rv)

-- | Operadores unarios
evalUnOp :: UnOp -> Value -> IO Value
evalUnOp OpNeg (VInt   n) = return $ VInt   (-n)
evalUnOp OpNeg (VFloat f) = return $ VFloat (-f)
evalUnOp OpNot (VBool  b) = return $ VBool  (not b)
evalUnOp _     v          = throwIO $ TypeMismatch $
    "tipo incorrecto para operador unario: " <> T.pack (show v)

-- | Convierte un Value a Text HTML (para interpolaciones)
valueToHtml :: Value -> IO Text
valueToHtml VUnit          = return ""
valueToHtml (VString s)    = return s
valueToHtml (VInt    n)    = return (T.pack $ show n)
valueToHtml (VFloat  f)    = return (T.pack $ show f)
valueToHtml (VBool True)   = return "true"
valueToHtml (VBool False)  = return "false"
valueToHtml (VHtml   h)    = return h
valueToHtml (VList   vs)   = T.concat <$> mapM valueToHtml vs
valueToHtml (VIO action)   = action >>= valueToHtml
valueToHtml other          = return $ T.pack (show other)