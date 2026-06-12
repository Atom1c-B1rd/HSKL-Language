module Interpreter.Builtins where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Control.Exception (throwIO)
import Interpreter.Value

-- ─── CÓMO AGREGAR UN BUILTIN ─────────────────────────────────────────────────
--
-- Un builtin es simplemente un Value en el entorno inicial.
-- Para agregar uno nuevo:
--
-- 1. Agregalo a `builtins` abajo con su nombre y valor
-- 2. Si toma 1 argumento:
--      ("nombre", VFun $ \v -> ...)
-- 3. Si toma 2 argumentos (currying):
--      ("nombre", VFun $ \a -> return $ VFun $ \b -> ...)
-- 4. Si es IO:
--      ("nombre", VFun $ \v -> return $ VIO $ do ...)
--
-- ─────────────────────────────────────────────────────────────────────────────

builtins :: [(Text, Value)]
builtins =
    -- IO / Console
    [ ("print",      mkFun1 builtinPrint)
    , ("consoleLog", mkFun1 builtinConsoleLog)
    , ("show",       mkFun1 builtinShow)

    -- Strings
    , ("concat",     mkFun1 builtinConcat)
    , ("length",     mkFun1 builtinLength)
    , ("toUpper",    mkFun1 builtinToUpper)
    , ("toLower",    mkFun1 builtinToLower)
    , ("words",      mkFun1 builtinWords)
    , ("unwords",    mkFun1 builtinUnwords)
    , ("lines",      mkFun1 builtinLines)
    , ("unlines",    mkFun1 builtinUnlines)

    -- Números
    , ("toInt",      mkFun1 builtinToInt)
    , ("toFloat",    mkFun1 builtinToFloat)
    , ("abs",        mkFun1 builtinAbs)
    , ("floor",      mkFun1 builtinFloor)
    , ("ceiling",    mkFun1 builtinCeiling)
    , ("round",      mkFun1 builtinRound)

    -- Listas
    , ("map",        mkFun2 builtinMap)
    , ("filter",     mkFun2 builtinFilter)
    , ("foldl",      mkFun3 builtinFoldl)
    , ("foldr",      mkFun3 builtinFoldr)
    , ("head",       mkFun1 builtinHead)
    , ("tail",       mkFun1 builtinTail)
    , ("null",       mkFun1 builtinNull)
    , ("reverse",    mkFun1 builtinReverse)
    , ("zip",        mkFun2 builtinZip)
    , ("take",       mkFun2 builtinTake)
    , ("drop",       mkFun2 builtinDrop)

    -- Ref (solo @client)
    , ("ref",        mkFun1 builtinRef)
    , ("readRef",    mkFun1 builtinReadRef)
    , ("writeRef",   mkFun2 builtinWriteRef)
    , ("modifyRef",  mkFun2 builtinModifyRef)

    -- Maybe
    , ("Just",       mkFun1 builtinJust)
    , ("Nothing",    VCon "Nothing" [])
    , ("fromMaybe",  mkFun2 builtinFromMaybe)
    , ("isJust",     mkFun1 builtinIsJust)
    , ("isNothing",  mkFun1 builtinIsNothing)
    -- Response helpers
    , ("ok",         mkFun1 builtinOk)
    , ("created",    mkFun1 builtinCreated)
    , ("noContent",  VResponse 204 VUnit)
    , ("notFound",   mkFun1 builtinNotFound)
    , ("badRequest", mkFun1 builtinBadRequest)
    , ("redirect",   mkFun1 builtinRedirect)
    -- Router
    , ("router",     mkFun2 builtinRouter)
    , ("get",    mkFun2 $ builtinRouteMethod "GET")
    , ("post",   mkFun2 $ builtinRouteMethod "POST")
    , ("put",    mkFun2 $ builtinRouteMethod "PUT")
    , ("patch",  mkFun2 $ builtinRouteMethod "PATCH")
    , ("delete", mkFun2 $ builtinRouteMethod "DELETE")
    -- Request accessors
    , ("getParam",   mkFun2 builtinGetParam)
    , ("getQuery",   mkFun2 builtinGetQuery)
    , ("getBody",    mkFun1 builtinGetBody)
    , ("getHeader",  mkFun2 builtinGetHeader)
    -- Control
    , ("error",      mkFun1 builtinError)
    , ("undefined",  VIO $ throwIO $ UserError "undefined")
    , ("return",     mkFun1 builtinReturn)
    ]

-- ─── HELPERS PARA CREAR BUILTINS ─────────────────────────────────────────────

-- | Función de 1 argumento
mkFun1 :: (Value -> IO Value) -> Value
mkFun1 f = VFun f

-- | Función de 2 argumentos (curried)
mkFun2 :: (Value -> Value -> IO Value) -> Value
mkFun2 f = VFun $ \a -> return $ VFun $ \b -> f a b

-- | Función de 3 argumentos (curried)
mkFun3 :: (Value -> Value -> Value -> IO Value) -> Value
mkFun3 f = VFun $ \a -> return $ VFun $ \b -> return $ VFun $ \c -> f a b c

-- ─── IO / CONSOLE ────────────────────────────────────────────────────────────

builtinPrint :: Value -> IO Value
builtinPrint v = return $ VIO $ do
    putStrLn (show v)
    return VUnit

builtinConsoleLog :: Value -> IO Value
builtinConsoleLog v = return $ VIO $ do
    putStrLn $ "[client] " ++ show v
    return VUnit

builtinShow :: Value -> IO Value
builtinShow v = return $ VString $ T.pack (show v)

-- ─── STRINGS ─────────────────────────────────────────────────────────────────

builtinConcat :: Value -> IO Value
builtinConcat (VList vs) = do
    strs <- mapM expectString vs
    return $ VString (T.concat strs)
builtinConcat (VString s) = return $ VString s
builtinConcat v = throwIO $ TypeMismatch $ "concat espera String o [String]"

builtinLength :: Value -> IO Value
builtinLength (VString s) = return $ VInt (T.length s)
builtinLength (VList   l) = return $ VInt (length l)
builtinLength v = throwIO $ TypeMismatch "length espera String o List"

builtinToUpper :: Value -> IO Value
builtinToUpper (VString s) = return $ VString (T.toUpper s)
builtinToUpper v = throwIO $ TypeMismatch "toUpper espera String"

builtinToLower :: Value -> IO Value
builtinToLower (VString s) = return $ VString (T.toLower s)
builtinToLower v = throwIO $ TypeMismatch "toLower espera String"

builtinWords :: Value -> IO Value
builtinWords (VString s) = return $ VList (map VString $ T.words s)
builtinWords v = throwIO $ TypeMismatch "words espera String"

builtinUnwords :: Value -> IO Value
builtinUnwords (VList vs) = do
    strs <- mapM expectString vs
    return $ VString (T.unwords strs)
builtinUnwords v = throwIO $ TypeMismatch "unwords espera [String]"

builtinLines :: Value -> IO Value
builtinLines (VString s) = return $ VList (map VString $ T.lines s)
builtinLines v = throwIO $ TypeMismatch "lines espera String"

builtinUnlines :: Value -> IO Value
builtinUnlines (VList vs) = do
    strs <- mapM expectString vs
    return $ VString (T.unlines strs)
builtinUnlines v = throwIO $ TypeMismatch "unlines espera [String]"

-- ─── NÚMEROS ─────────────────────────────────────────────────────────────────

builtinToInt :: Value -> IO Value
builtinToInt (VFloat f) = return $ VInt (floor f)
builtinToInt (VString s) = case reads (T.unpack s) of
    [(n, "")] -> return $ VInt n
    _         -> throwIO $ TypeMismatch $ "no se puede convertir a Int: " <> s
builtinToInt v@(VInt _) = return v
builtinToInt v = throwIO $ TypeMismatch "toInt espera número o String"

builtinToFloat :: Value -> IO Value
builtinToFloat (VInt n)   = return $ VFloat (fromIntegral n)
builtinToFloat v@(VFloat _) = return v
builtinToFloat v = throwIO $ TypeMismatch "toFloat espera número"

builtinAbs :: Value -> IO Value
builtinAbs (VInt   n) = return $ VInt   (abs n)
builtinAbs (VFloat f) = return $ VFloat (abs f)
builtinAbs v = throwIO $ TypeMismatch "abs espera número"

builtinFloor :: Value -> IO Value
builtinFloor (VFloat f) = return $ VInt (floor f)
builtinFloor v@(VInt _) = return v
builtinFloor v = throwIO $ TypeMismatch "floor espera número"

builtinCeiling :: Value -> IO Value
builtinCeiling (VFloat f) = return $ VInt (ceiling f)
builtinCeiling v@(VInt _) = return v
builtinCeiling v = throwIO $ TypeMismatch "ceiling espera número"

builtinRound :: Value -> IO Value
builtinRound (VFloat f) = return $ VInt (round f)
builtinRound v@(VInt _) = return v
builtinRound v = throwIO $ TypeMismatch "round espera número"

-- ─── LISTAS ──────────────────────────────────────────────────────────────────

builtinMap :: Value -> Value -> IO Value
builtinMap (VFun f) (VList vs) = VList <$> mapM f vs
builtinMap _ _ = throwIO $ TypeMismatch "map espera función y lista"

builtinFilter :: Value -> Value -> IO Value
builtinFilter (VFun f) (VList vs) = do
    results <- mapM (\v -> f v >>= expectBool) vs
    return $ VList [v | (v, True) <- zip vs results]
builtinFilter _ _ = throwIO $ TypeMismatch "filter espera función y lista"

builtinFoldl :: Value -> Value -> Value -> IO Value
builtinFoldl (VFun f) acc (VList vs) =
    foldl step (return acc) vs
  where
    step ioAcc v = do
        a   <- ioAcc
        fv  <- f a
        case fv of
            VFun g -> g v
            _      -> throwIO $ TypeMismatch "foldl: función debe tomar 2 args"
builtinFoldl _ _ _ = throwIO $ TypeMismatch "foldl espera función, acumulador y lista"

builtinFoldr :: Value -> Value -> Value -> IO Value
builtinFoldr (VFun f) acc (VList vs) =
    foldr step (return acc) vs
  where
    step v ioAcc = do
        a   <- ioAcc
        fv  <- f v
        case fv of
            VFun g -> g a
            _      -> throwIO $ TypeMismatch "foldr: función debe tomar 2 args"
builtinFoldr _ _ _ = throwIO $ TypeMismatch "foldr espera función, acumulador y lista"

builtinHead :: Value -> IO Value
builtinHead (VList (x:_)) = return x
builtinHead (VList [])    = throwIO $ UserError "head: lista vacía"
builtinHead v = throwIO $ TypeMismatch "head espera lista"

builtinTail :: Value -> IO Value
builtinTail (VList (_:xs)) = return $ VList xs
builtinTail (VList [])     = throwIO $ UserError "tail: lista vacía"
builtinTail v = throwIO $ TypeMismatch "tail espera lista"

builtinNull :: Value -> IO Value
builtinNull (VList   []) = return $ VBool True
builtinNull (VList   _ ) = return $ VBool False
builtinNull (VString s ) = return $ VBool (T.null s)
builtinNull v = throwIO $ TypeMismatch "null espera lista o String"

builtinReverse :: Value -> IO Value
builtinReverse (VList   vs) = return $ VList   (reverse vs)
builtinReverse (VString s ) = return $ VString (T.reverse s)
builtinReverse v = throwIO $ TypeMismatch "reverse espera lista o String"

builtinZip :: Value -> Value -> IO Value
builtinZip (VList as) (VList bs) =
    return $ VList [VTuple [a, b] | (a, b) <- zip as bs]
builtinZip _ _ = throwIO $ TypeMismatch "zip espera dos listas"

builtinTake :: Value -> Value -> IO Value
builtinTake (VInt n) (VList vs)   = return $ VList   (take n vs)
builtinTake (VInt n) (VString s)  = return $ VString (T.take n s)
builtinTake _ _ = throwIO $ TypeMismatch "take espera Int y lista"

builtinDrop :: Value -> Value -> IO Value
builtinDrop (VInt n) (VList vs)   = return $ VList   (drop n vs)
builtinDrop (VInt n) (VString s)  = return $ VString (T.drop n s)
builtinDrop _ _ = throwIO $ TypeMismatch "drop espera Int y lista"

-- ─── REF ─────────────────────────────────────────────────────────────────────

builtinRef :: Value -> IO Value
builtinRef v = VRef <$> newIORef v

builtinReadRef :: Value -> IO Value
builtinReadRef (VRef r) = return $ VIO $ readIORef r
builtinReadRef v = throwIO $ TypeMismatch "readRef espera Ref"

builtinWriteRef :: Value -> Value -> IO Value
builtinWriteRef (VRef r) v = return $ VIO $ do
    writeIORef r v
    return VUnit
builtinWriteRef _ _ = throwIO $ TypeMismatch "writeRef espera Ref"

builtinModifyRef :: Value -> Value -> IO Value
builtinModifyRef (VRef r) (VFun f) = return $ VIO $ do
    v  <- readIORef r
    v' <- f v
    writeIORef r v'
    return VUnit
builtinModifyRef _ _ = throwIO $ TypeMismatch "modifyRef espera Ref y función"

-- ─── MAYBE ───────────────────────────────────────────────────────────────────

builtinJust :: Value -> IO Value
builtinJust v = return $ VCon "Just" [v]

builtinFromMaybe :: Value -> Value -> IO Value
builtinFromMaybe def (VCon "Nothing" []) = return def
builtinFromMaybe _   (VCon "Just" [v])  = return v
builtinFromMaybe _ v = throwIO $ TypeMismatch $ "fromMaybe espera Maybe"

builtinIsJust :: Value -> IO Value
builtinIsJust (VCon "Just"    _) = return $ VBool True
builtinIsJust (VCon "Nothing" _) = return $ VBool False
builtinIsJust v = throwIO $ TypeMismatch "isJust espera Maybe"

builtinIsNothing :: Value -> IO Value
builtinIsNothing v = do
    VBool b <- builtinIsJust v
    return $ VBool (not b)

-- ─── CONTROL ─────────────────────────────────────────────────────────────────

builtinError :: Value -> IO Value
builtinError (VString msg) = throwIO $ UserError msg
builtinError v = throwIO $ UserError (T.pack $ show v)

builtinReturn :: Value -> IO Value
builtinReturn v = return $ VIO (return v)

-- ─── HELPERS INTERNOS ────────────────────────────────────────────────────────

expectString :: Value -> IO Text
expectString (VString s) = return s
expectString v = throwIO $ TypeMismatch $ "esperaba String, obtuve: " <> T.pack (show v)

expectBool :: Value -> IO Bool
expectBool (VBool b) = return b
expectBool v = throwIO $ TypeMismatch $ "esperaba Bool, obtuve: " <> T.pack (show v)

expectInt :: Value -> IO Int
expectInt (VInt n) = return n
expectInt v = throwIO $ TypeMismatch $ "esperaba Int, obtuve: " <> T.pack (show v)


-- ─── RESPONSE ────────────────────────────────────────────────────────────────

builtinOk :: Value -> IO Value
builtinOk v = return $ VResponse 200 v

builtinCreated :: Value -> IO Value
builtinCreated v = return $ VResponse 201 v

builtinNotFound :: Value -> IO Value
builtinNotFound v = return $ VResponse 404 v

builtinBadRequest :: Value -> IO Value
builtinBadRequest v = return $ VResponse 400 v

builtinRedirect :: Value -> IO Value
builtinRedirect (VString url) = return $ VResponse 302 (VString url)
builtinRedirect v = throwIO $ TypeMismatch "redirect espera String"

-- ─── ROUTER ──────────────────────────────────────────────────────────────────
builtinRouteMethod :: Text -> Value -> Value -> IO Value
builtinRouteMethod method handler (VString path) =
    return $ VRouter $ RouteEntry method path handler 
builtinRouteMethod _ _ _ = throwIO $ TypeMismatch "get/post/... espera handler y String"

builtinRouter :: Value -> Value -> IO Value
builtinRouter handler (VString path) =
    return $ VRouter $ RouteEntry "" path handler
builtinRouter _ _ = throwIO $ TypeMismatch "router espera handler y String"

-- ─── REQUEST ACCESSORS ───────────────────────────────────────────────────────

builtinGetParam :: Value -> Value -> IO Value
builtinGetParam (VRequest req) (VString key) =
    return $ case lookup key (reqParams req) of
        Just v  -> VCon "Just" [VString v]
        Nothing -> VCon "Nothing" []
builtinGetParam (VString key) _ =
    throwIO $ TypeMismatch "getParam: primer argumento debe ser Request"
builtinGetParam _ _ = throwIO $ TypeMismatch "getParam espera Request y String"

builtinGetQuery :: Value -> Value -> IO Value
builtinGetQuery (VRequest req) (VString key) =
    return $ case lookup key (reqQuery req) of
        Just v  -> VCon "Just" [VString v]
        Nothing -> VCon "Nothing" []
builtinGetQuery _ _ = throwIO $ TypeMismatch "getQuery espera Request y String"

builtinGetBody :: Value -> IO Value
builtinGetBody (VRequest req) =
    return $ case reqBody req of
        Just b  -> VCon "Just" [VString b]
        Nothing -> VCon "Nothing" []
builtinGetBody _ = throwIO $ TypeMismatch "getBody espera Request"

builtinGetHeader :: Value -> Value -> IO Value
builtinGetHeader (VRequest req) (VString key) =
    return $ case lookup key (reqHeaders req) of
        Just v  -> VCon "Just" [VString v]
        Nothing -> VCon "Nothing" []
builtinGetHeader _ _ = throwIO $ TypeMismatch "getHeader espera Request y String"