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
runHsklFile :: ServerConfig -> FilePath -> [(Text, Text)] -> IO (Either String (String, [RouteEntry]))
runHsklFile config filePath urlParams = do
    let ctx = defaultCtx { ctxUrlParams = urlParams }
    result <- try $ do
        source <- TIO.readFile filePath
        prog   <- parseOrFail filePath source
        runProgram config ctx prog
    return $ case result of
        Left  (e :: SomeException)  -> Left (show e)
        Right (html, routers)       -> Right (T.unpack html, routers)

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
runProgram :: ServerConfig -> RequestCtx -> Program -> IO (Text, [RouteEntry])
runProgram config ctx (Program sections) = do
    let baseEnv = buildBaseEnv ctx
    (_, html, clientFuncs, routers) <- foldSections baseEnv sections
    let script = generateScript clientFuncs
    return (html <> script, routers)
  where
    foldSections env [] = return (env, "", [], [])
    foldSections env (sec : rest) = do
        (env', piece, fds, routes) <- runSection config ctx env sec
        (envFinal, restHtml, restFds, restRoutes) <- foldSections env' rest
        return (envFinal, piece <> restHtml, fds ++ restFds, routes ++ restRoutes)

-- | Procesa una sección del archivo
-- Devuelve (entorno actualizado, HTML generado, funciones @client, routers)
runSection :: ServerConfig -> RequestCtx -> Env -> Section -> IO (Env, Text, [FuncDecl], [RouteEntry])
runSection _ _ env (SHtml raw) =
    return (env, raw, [], [])

runSection config ctx env (SCode decls) = do
    (env', fds, routers) <- foldDecls config ctx env decls
    return (env', "", fds, routers)

runSection _ _ env (SInterp expr) = do
    val  <- eval env expr
    h    <- valueToHtml val
    return (env, h, [], [])



foldDecls :: ServerConfig -> RequestCtx -> Env -> [Decl] -> IO (Env, [FuncDecl], [RouteEntry])
foldDecls _ _ env [] = return (env, [], [])
foldDecls config ctx env (decl : rest) = do
    (env', mfd, mroute) <- evalDecl config ctx env decl
    (envFinal, fds, routes) <- foldDecls config ctx env' rest
    return (envFinal, maybe fds (:fds) mfd, maybe routes (:routes) mroute)

evalDecl :: ServerConfig -> RequestCtx -> Env -> Decl -> IO (Env, Maybe FuncDecl, Maybe RouteEntry)
evalDecl config ctx env (DFunc fd)
    | funcFlag fd == Just FClient =
        return (env, Just fd, Nothing)
    | otherwise = do
        val <- makeFuncValue env fd
        let env' = extendEnv (funcName fd) val env
        -- si el valor es un VRouter, lo extraemos
        case val of
            VRouter route -> return (env', Nothing, Just route)
            _             -> return (env', Nothing, Nothing)

evalDecl _ _ env (DData dd) = do
    let constructors = concatMap makeConstructor (dataCons dd)
    return (extendEnvMany constructors env, Nothing, Nothing)

evalDecl config ctx env (DClass cd) = do
    let members = classMembers cd
    memberVals <- mapM (evalClassMember env) members
    return (extendEnvMany memberVals env, Nothing, Nothing)

evalDecl _ _ env (DImport _) = return (env, Nothing, Nothing)

-- ─── CONSTRUCCIÓN DE FUNCIONES ───────────────────────────────────────────────

-- | Convierte una FuncDecl en un Value que el intérprete puede usar
-- La clave es crear un closure que capture el entorno actual
makeFuncValue :: Env -> FuncDecl -> IO Value
makeFuncValue env fd =
    case funcCases fd of
        []                          -> throwIO $ UserError $ "función sin casos: " <> funcName fd
        [FuncCase [] body]          -> eval env body
        cases                       -> return $ VFun $ \arg -> applyFuncCases env cases [arg]


-- | Construye un constructor de data como Value
-- Si tiene campos, es una función; si no, es un VCon directo
makeConstructor :: Constructor -> [(Text, Value)]
makeConstructor (Constructor name [] []) =
    [(name, VCon name [])]
makeConstructor (Constructor name fields []) =
    [(name, buildConFun name (length fields))]
makeConstructor (Constructor name fields records) =
    -- el constructor mismo
    (name, buildRecordConFun name (map fst records))
    -- un accessor por cada campo
    : [ (fname, VFun $ \case
            VCon n vs | n == name ->
                case lookup fname (zip (map fst records) vs) of
                    Just v  -> return v
                    Nothing -> throwIO $ UserError $ "campo no encontrado: " <> fname
            other -> throwIO $ TypeMismatch $ "esperaba " <> name)
      | (fname, _) <- records
      ]
-- | Crea una función que acumula N argumentos y devuelve el VCon
buildConFun :: Text -> Int -> Value
buildConFun name 0    = VCon name []
buildConFun name n    = go name n []
  where
    go name 0 acc = VCon name (reverse acc)
    go name n acc = VFun $ \v -> return $ go name (n-1) (v:acc)

buildRecordConFun :: Text -> [Text] -> Value
buildRecordConFun name fields = go fields []
  where
    go []     acc = VCon name (reverse acc)
    go (_:fs) acc = VFun $ \v -> return $ go fs (v:acc)

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
        [ ("_method",  VString $ ctxMethod ctx)
        , ("_params",  VList $ map pairToVal $ ctxUrlParams ctx)
        , ("_query",   VList $ map pairToVal $ ctxQuery ctx)
        , ("_body",    maybe VUnit VString $ ctxBody ctx)
        , ("_headers", VList $ map pairToVal $ ctxHeaders ctx)
        , ("request",  VRequest $ RequestData
                { reqMethod  = ctxMethod ctx
                , reqParams  = ctxUrlParams ctx
                , reqQuery   = ctxQuery ctx
                , reqBody    = ctxBody ctx
                , reqHeaders = ctxHeaders ctx
                })
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