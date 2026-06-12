module Interpreter.Value where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.IORef ( IORef )
import Control.Exception (Exception)

-- ─── VALORES EN RUNTIME ──────────────────────────────────────────────────────

data Value
    = VInt    Int
    | VFloat  Double
    | VString Text
    | VBool   Bool
    | VUnit
    | VList   [Value]
    | VTuple  [Value]
    | VRef    (IORef Value)         -- @state / ref, solo en @client
    | VHtml   Text                  -- resultado HTML renderizado
    | VIO     (IO Value)            -- acción IO sin ejecutar
    | VResponse Int Value
    | VRouter RouteEntry
    | VRequest  RequestData
    | VFun    (Value -> IO Value)
    -- Constructor de data: guarda su nombre y campos ya evaluados
    | VCon    Text [Value]

data RouteEntry = RouteEntry
    { routeMethod  :: Text         -- "GET", "POST", etc.
    , routeEntryPath    :: Text         -- "/users/:id"
    , routeHandler :: Value        -- la función handler
    } deriving (Show)

data RequestData = RequestData
    { reqMethod  :: Text
    , reqParams  :: [(Text, Text)]
    , reqQuery   :: [(Text, Text)]
    , reqBody    :: Maybe Text
    , reqHeaders :: [(Text, Text)]
    } deriving (Show)
-- VFun no puede derivar Show automaticamente porque contiene una función
-- asi que lo hacemos manual
instance Show Value where
    show (VInt    n)   = show n
    show (VFloat  f)   = show f
    show (VString s)   = T.unpack s
    show (VBool   b)   = if b then "True" else "False"
    show VUnit         = "()"
    show (VList   vs)  = "[" ++ unwords (map show vs) ++ "]"
    show (VTuple  vs)  = "(" ++ unwords (map show vs) ++ ")"
    show (VRef    _)   = "<ref>"
    show (VHtml   h)   = T.unpack h
    show (VIO     _)   = "<io>"
    show (VFun    _)   = "<function>"
    show (VCon n [])   = T.unpack n
    show (VCon n vs)   = T.unpack n ++ " " ++ unwords (map show vs)
    show (VResponse code _) = "<response " ++ show code ++ ">"
    show (VRouter r) = "<router " ++ T.unpack (routeEntryPath r) ++ ">"
    show (VRequest  _)      = "<request>"
    

-- ─── ENTORNO ─────────────────────────────────────────────────────────────────

-- El entorno es simplemente un Map de nombre a valor
-- Usamos Map para buscar en O(log n)
type Env = Map Text Value

-- | Entorno vacío
emptyEnv :: Env
emptyEnv = Map.empty

-- | Buscar una variable en el entorno
lookupVar :: Text -> Env -> Either RuntimeError Value
lookupVar name env =
    case Map.lookup name env of
        Just v  -> Right v
        Nothing -> Left (UndefinedVar name)

-- | Agregar una variable al entorno (shadowing: la nueva tapa la vieja)
extendEnv :: Text -> Value -> Env -> Env
extendEnv = Map.insert

-- | Agregar muchas variables de una vez (para let, where, args)
extendEnvMany :: [(Text, Value)] -> Env -> Env
extendEnvMany bindings env = foldr (uncurry Map.insert) env bindings

-- ─── ERRORES ─────────────────────────────────────────────────────────────────

data RuntimeError
    = UndefinedVar   Text           -- variable no encontrada
    | TypeMismatch   Text           -- tipo incorrecto: "expected Int, got String"
    | ArityError     Text           -- cantidad de argumentos incorrecta
    | DivByZero                     -- división por cero
    | PatternFail    Text           -- ningún patrón en case matcheó
    | RefInServer    Text           -- se intentó usar Ref en @server
    | IOError'       Text           -- error de IO
    | UserError      Text           -- error del usuario (error "msg")
    deriving (Show, Eq)

instance Exception RuntimeError

-- | Helper para mostrar errores bonito
showError :: RuntimeError -> String
showError (UndefinedVar   n) = "Variable no definida: " ++ T.unpack n
showError (TypeMismatch   m) = "Error de tipo: " ++ T.unpack m
showError (ArityError     m) = "Error de aridad: " ++ T.unpack m
showError DivByZero          = "División por cero"
showError (PatternFail    m) = "Pattern match fallido: " ++ T.unpack m
showError (RefInServer    n) = "No se puede usar Ref en @server: " ++ T.unpack n
showError (IOError'       m) = "Error de IO: " ++ T.unpack m
showError (UserError      m) = "Error: " ++ T.unpack m
