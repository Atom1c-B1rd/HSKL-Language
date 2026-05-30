module Lexer.Tokens where

import Data.Text (Text)

-- | Posición en el archivo
data Pos = Pos
    { posLine   :: Int
    , posCol    :: Int
    } deriving (Show, Eq)

-- | Token con posición
data Located a = Located
    { locPos :: Pos
    , locVal :: a
    } deriving (Show, Eq)

type LToken = Located Token

-- | Todos los tokens de HSKL
data Token
    -- Delimitadores de template
    = TOpenCode        -- <?hs
    | TCloseCode       -- ?>
    | TOpenInterp      -- <?=
    | TCloseInterp     -- ?>
    | TOpenComp  Text   -- <Layout>
    | TCloseComp Text   -- </Layout>
    | THtml Text       -- contenido HTML literal

    -- Keywords Haskell
    | TIf
    | TThen
    | TElse
    | TLet
    | TIn
    | TWhere
    | TCase
    | TOf
    | TDo

    -- Keywords HSKL
    | TData
    | TClass
    | TImpl
    | TImport
    | TModule

    -- Flags / Anotaciones
    | TAt Flag

    -- Literales
    | TInt    Int
    | TFloat  Double
    | TString Text
    | TChar   Char
    | TBool   Bool
    | TUnit            -- ()

    -- Identificadores
    | TIdent  Text     -- identificador normal: miVar
    | TConstr Text     -- constructor/tipo: MiTipo

    -- Tipos builtin
    | TTypeString
    | TTypeInt
    | TTypeFloat
    | TTypeBool
    | TTypeChar
    | TTypeMaybe
    | TTypeEither
    | TTypeList
    | TTypeMap
    | TTypeIO
    | TTypeRequest
    | TTypeResponse
    | TTypeRef
    | TTypeVoid        -- por ahora lo mantenemos, despues vemos

    -- Operadores de tipo
    | TDoubleColon     -- ::
    | TArrow           -- ->
    | TFatArrow        -- =>

    -- Definicion
    | TEquals          -- =

    -- Operadores aritmeticos
    | TPlus            -- +
    | TMinus           -- -
    | TStar            -- *
    | TSlash           -- /
    | TCaret           -- ^
    | TMod             -- mod
    | TDiv             -- div

    -- Operadores de comparacion
    | TEqEq            -- ==
    | TNotEq           -- /=
    | TLt              -- <
    | TGt              -- >
    | TLtEq            -- <=
    | TGtEq            -- >=

    -- Operadores logicos
    | TAnd             -- &&
    | TOr              -- ||
    | TNot             -- not

    -- Operadores de funcion
    | TDot             -- .
    | TDollar          -- $
    | TFmap            -- <$>
    | TAp              -- <*>
    | TBind            -- >>=
    | TThen'           -- >>  (TThen' para no chocar con TThen)
    | TBackslash       -- \   lambda

    -- Operadores de string/lista
    | TConcat          -- ++
    | TCons            -- :

    -- Agrupacion
    | TLParen          -- (
    | TRParen          -- )
    | TLBracket        -- [
    | TRBracket        -- ]
    | TLBrace          -- {
    | TRBrace          -- }

    -- Separadores
    | TComma           -- ,
    | TSemicolon       -- ;
    | TPipe            -- |
    | TUnderscore      -- _
    | TDotDot          -- ..

    -- Layout virtual (insertados por el lexer)
    | TVOpen           -- { virtual
    | TVClose          -- } virtual
    | TVSep            -- ; virtual

    -- Wildcard / especiales
    | TEOF
    deriving (Show, Eq)

-- | Flags de anotacion
data Flag
    = FClient
    | FServer
    | FState
    | FPublic
    | FPrivate
    deriving (Show, Eq)
