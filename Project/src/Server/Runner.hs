module Server.Runner where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Exception (try, SomeException, throwIO)

import Parser.AST
import Parser.Parser (parseProgram)
import Interpreter.Eval
import Interpreter.Value
import Interpreter.Builtins
import Server.ServerConfig
import Lexer.Tokens (Flag(..))
import Transpiler.JsGen (generateScript, transpileDecl)

import Text.Megaparsec (parse, errorBundlePretty)

-- ─── CONTEXTO DE EJECUCIÓN ───────────────────────────────────────────────────

-- | El contexto que recibe cada archivo al ejecutarse
-- Es lo que en PHP sería $_GET, $_POST, $_SERVER, etc.
data RequestCtx = RequestCtx
    { ctxMethod   :: Text               -- "GET", "POST", etc.
    , ctxUrlParams :: [(Text, Text)]    -- parámetros de la URL: /blog/:slug
    , ctxQuery    :: [(Text, Text)]     -- query string: ?foo=bar
    , ctxBody     :: Maybe Text         -- body del request (POST)
    , ctxHeaders  :: [(Text, Text)]     -- headers HTTP
    } deriving (Show)

defaultCtx :: RequestCtx
defaultCtx = RequestCtx "GET" [] [] Nothing []

-- ─── ENTRY POINT DEL RUNNER ──────────────────────────────────────────────────

-- | Lee un archivo .hskl, lo parsea, lo interpreta y devuelve el HTML final
-- Este es el corazón de HSKL: lo que PHP hace por defecto
runHsklFile :: ServerConfig -> FilePath -> [(Text, Text)] -> IO (Either String String)
runHsklFile config filePath urlParams = do
    let ctx = defaultCtx { ctxUrlParams = urlParams }
    result <- try $ do
        -- 1. Leer el archivo
        source <- TIO.readFile filePath
        -- 2. Parsear
        prog   <- parseOrFail filePath source
        -- 3. Interpretar y renderizar
        runProgram config ctx prog
    return $ case result of
        Left  (e :: SomeException) -> Left (show e)
        Right html                 -> Right (T.unpack html)

-- ─── PARSEO ──────────────────────────────────────────────────────────────────

parseOrFail :: FilePath -> Text -> IO Program
parseOrFail filePath source =
    case parse parseProgram filePath source of
        Left  err  -> throwIO $ userError $ errorBundlePretty err
        Right prog -> return prog

-- ─── INTÉRPRETE DE PROGRAMA ──────────────────────────────────────────────────

-- | Interpreta un Program completo y devuelve el HTML renderizado
-- Recorre las secciones en orden:
--   SHtml   → va directo al output
--   SCode   → evalúa las declaraciones, actualiza el entorno
--   SInterp → evalúa la expresión y agrega el resultado al output
runProgram :: ServerConfig -> RequestCtx -> Program -> IO Text
runProgram config ctx (Program sections) = do
    let baseEnv = buildBaseEnv ctx
    (_, html, clientFuncs) <- foldSections baseEnv sections
    let script = generateScript clientFuncs
    return (html <> script)
  where
    foldSections env [] = return (env, "", [])
    foldSections env (sec : rest) = do
        (env', piece, fds) <- runSection config ctx env sec
        (envFinal, restHtml, restFds) <- foldSections env' rest
        return (envFinal, piece <> restHtml, fds ++ restFds)

-- | Procesa una sección del archivo
-- Devuelve (entorno actualizado, HTML generado, funciones @client)
runSection :: ServerConfig -> RequestCtx -> Env -> Section -> IO (Env, Text, [FuncDecl])

runSection _ _ env (SHtml raw) = return (env, raw, [])

runSection config ctx env (SCode decls) = do
    (env', fds) <- foldDecls config ctx env decls
    return (env', "", fds)

runSection config ctx env (SInterp expr) = do
    val  <- eval env expr
    html <- valueToHtml val
    return (env, html, [])

-- ─── DECLARACIONES ───────────────────────────────────────────────────────────

-- | Evalúa una lista de declaraciones y actualiza el entorno
-- Devuelve también las funciones @client para transpilar
foldDecls :: ServerConfig -> RequestCtx -> Env -> [Decl] -> IO (Env, [FuncDecl])
foldDecls _      _   env []           = return (env, [])
foldDecls config ctx env (decl : rest) = do
    (env', mfd) <- evalDecl config ctx env decl
    (envFinal, fds) <- foldDecls config ctx env' rest
    return (envFinal, maybe fds (:fds) mfd)

-- | Evalúa una declaración y la agrega al entorno
-- Devuelve (env actualizado, Just fd si es @client para transpilar)
evalDecl :: ServerConfig -> RequestCtx -> Env -> Decl -> IO (Env, Maybe FuncDecl)

evalDecl config ctx env (DFunc fd)
    -- @client: no evalúa en servidor, lo guarda para el transpilador
    | funcFlag fd == Just FClient =
        return (env, Just fd)
    -- @server o sin flag: evalúa normal
    | otherwise = do
        val <- makeFuncValue env fd
        return (extendEnv (funcName fd) val env, Nothing)

evalDecl _ _ env (DData dd) = do
    let constructors = map makeConstructor (dataCons dd)
    return (extendEnvMany constructors env, Nothing)

evalDecl config ctx env (DClass cd) = do
    let members = classMembers cd
    memberVals <- mapM (evalClassMember env) members
    return (extendEnvMany memberVals env, Nothing)

evalDecl _ _ env (DImport _) = return (env, Nothing)

-- ─── CONSTRUCCIÓN DE FUNCIONES ───────────────────────────────────────────────

-- | Convierte una FuncDecl en un Value que el intérprete puede usar
-- La clave es crear un closure que capture el entorno actual
makeFuncValue :: Env -> FuncDecl -> IO Value
makeFuncValue env fd =
    case funcArgs fd of
        -- Sin argumentos: evalúa directo, es un valor no una función
        [] -> eval env (funcBody fd)
        -- Con argumentos: crea función curried
        args -> return $ curryLam env args (funcBody fd)

-- | Construye un constructor de data como Value
-- Si tiene campos, es una función; si no, es un VCon directo
makeConstructor :: Constructor -> (Text, Value)
makeConstructor (Constructor name []) =
    (name, VCon name [])
makeConstructor (Constructor name fields) =
    (name, buildConFun name (length fields))

-- | Crea una función que acumula N argumentos y devuelve el VCon
buildConFun :: Text -> Int -> Value
buildConFun name 0    = VCon name []
buildConFun name n    = go name n []
  where
    go name 0 acc = VCon name (reverse acc)
    go name n acc = VFun $ \v -> return $ go name (n-1) (v:acc)

-- | Evalúa un miembro de clase
evalClassMember :: Env -> ClassMember -> IO (Text, Value)
evalClassMember env (ClassMember _ fd) = do
    val <- makeFuncValue env fd
    return (funcName fd, val)

-- ─── ENTORNO BASE ────────────────────────────────────────────────────────────

-- | Construye el entorno inicial con builtins + contexto HTTP
-- Acá es donde viven $_GET, $_POST, etc. de PHP pero en HSKL
buildBaseEnv :: RequestCtx -> Env
buildBaseEnv ctx =
    extendEnvMany httpVars $
    extendEnvMany builtins
    emptyEnv
  where
    httpVars =
        -- Equivalentes a las superglobales de PHP
        [ ("_method",  VString $ ctxMethod ctx)
        , ("_params",  VList $ map pairToVal $ ctxUrlParams ctx)
        , ("_query",   VList $ map pairToVal $ ctxQuery ctx)
        , ("_body",    maybe VUnit VString $ ctxBody ctx)
        , ("_headers", VList $ map pairToVal $ ctxHeaders ctx)
        -- Helpers para acceder a params individuales
        , ("getParam", makeGetParam (ctxUrlParams ctx))
        , ("getQuery", makeGetParam (ctxQuery ctx))
        ]
    pairToVal (k, v) = VTuple [VString k, VString v]

-- | Genera la función getParam que busca en los parámetros
makeGetParam :: [(Text, Text)] -> Value
makeGetParam pairs = VFun $ \case
    VString key ->
        return $ case lookup key pairs of
            Just v  -> VCon "Just" [VString v]
            Nothing -> VCon "Nothing" []
    _ -> throwIO $ TypeMismatch "getParam espera String"
  where
    throwIO = Control.Exception.throwIO

-- ─── RENDERIZADO A HTML ──────────────────────────────────────────────────────

-- | Convierte un Value a Text HTML para interpolar en el template
-- Equivalente al echo de PHP


-- | Escapa HTML para evitar XSS (igual que htmlspecialchars de PHP)
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '&'  = "&amp;"
    escape '"'  = "&quot;"
    escape '\'' = "&#39;"
    escape c    = T.singleton c

-- | Para HTML crudo sin escapar (equivalente a echo $html en PHP)
-- El usuario tiene que usar esto explícitamente, es más seguro
rawHtml :: Text -> Value
rawHtml = VHtml