module Parser.AST where

import Data.Text (Text)
import Lexer.Tokens (Flag)

-- ─── PROGRAMA ────────────────────────────────────────────────────────────────

-- | Un archivo .hskl es una lista de secciones
data Program = Program [Section]
    deriving (Show, Eq)

-- | Una sección es HTML puro o un bloque de código
data Section
    = SHtml   Text          -- contenido HTML literal
    | SCode   [Decl]        -- <?hs ... ?>
    | SInterp Expr          -- <?= expr ?>
    deriving (Show, Eq)

-- ─── DECLARACIONES ───────────────────────────────────────────────────────────

data Decl
    = DFunc   FuncDecl      -- función: flag? nombre :: tipo \n nombre args = expr
    | DData   DataDecl      -- data Nombre = Constructor | ...
    | DClass  ClassDecl     -- class Nombre impl Otro { ... }
    | DImport Text          -- import Modulo
    deriving (Show, Eq)

-- | Declaración de función
-- @client           <- flag opcional
-- hi :: String      <- firma de tipo
-- hi x = expr       <- definición
data FuncDecl = FuncDecl
    { funcFlag :: Maybe Flag    -- @client / @server / @state
    , funcName :: Text
    , funcType :: Maybe Type    -- la firma es opcional en el parser
    , funcArgs :: [Text]
    , funcBody :: Expr
    } deriving (Show, Eq)

-- | data Arbol = Hoja | Nodo Int Arbol Arbol
data DataDecl = DataDecl
    { dataName  :: Text
    , dataCons  :: [Constructor]
    } deriving (Show, Eq)

data Constructor = Constructor
    { conName   :: Text
    , conFields :: [Type]
    } deriving (Show, Eq)

-- | class Nombre impl Ejemplo { @public cosa :: String ... }
data ClassDecl = ClassDecl
    { className   :: Text
    , classImpl   :: Maybe Text     -- impl Otro
    , classMembers:: [ClassMember]
    } deriving (Show, Eq)

data ClassMember = ClassMember
    { memberFlag :: Maybe Flag      -- @public / @private
    , memberDecl :: FuncDecl
    } deriving (Show, Eq)

-- ─── TIPOS ───────────────────────────────────────────────────────────────────

data Type
    = TyCon  Text           -- Int, String, Bool
    | TyApp  Type Type      -- Maybe Int, IO String
    | TyFun  Type Type      -- String -> Int
    | TyList Type           -- [Int]
    | TyTuple [Type]        -- (Int, String)
    | TyUnit                -- ()
    | TyHtml
    deriving (Show, Eq)

-- ─── EXPRESIONES ─────────────────────────────────────────────────────────────

data Expr
    -- Literales
    = EInt    Int
    | EFloat  Double
    | EString Text
    | EChar   Char
    | EBool   Bool
    | EUnit

    -- Variables y constructores
    | EVar    Text          -- variable: nombre
    | ECon    Text          -- constructor: Nombre

    -- Aplicación de función: f x y
    -- En Haskell f x y es (App (App f x) y)
    | EApp    Expr Expr

    -- Lambda: \x -> expr
    | ELam    [Text] Expr

    -- Let: let x = e1 in e2
    | ELet    [(Text, Expr)] Expr

    -- Where: expr where { x = e1; y = e2 }
    | EWhere  Expr [(Text, Expr)]

    -- Case: case expr of { pat -> expr; ... }
    | ECase   Expr [CaseAlt]

    -- If: if e1 then e2 else e3
    | EIf     Expr Expr Expr

    -- Operadores binarios
    | EBinOp  BinOp Expr Expr

    -- Operador unario
    | EUnOp   UnOp Expr

    -- Lista: [1, 2, 3]
    | EList   [Expr]

    -- Tupla: (1, "hola")
    | ETuple  [Expr]

    -- Do notation
    | EDo     [DoStmt]

    -- IO especificos
    | ERef    Expr          -- ref valor

    | EHtml [HtmlNode]
    
    deriving (Show, Eq)

data HtmlNode
    = HtmlRaw  Text 
    | HtmlExpr Expr       
    | HtmlComp Text [HtmlNode]
    deriving (Show, Eq)

-- | Alternativa de case
data CaseAlt = CaseAlt
    { altPat  :: Pat
    , altExpr :: Expr
    } deriving (Show, Eq)

-- | Statement en do notation
data DoStmt
    = DoBind Text Expr      -- x <- accion
    | DoExpr Expr           -- accion (sin bind)
    | DoLet  Text Expr      -- let x = expr
    deriving (Show, Eq)

-- ─── PATRONES ────────────────────────────────────────────────────────────────

data Pat
    = PVar    Text          -- variable
    | PCon    Text [Pat]    -- Constructor p1 p2
    | PLit    Literal       -- literal
    | PWild               -- _
    | PTuple  [Pat]         -- (p1, p2)
    | PList   [Pat]         -- [p1, p2]
    deriving (Show, Eq)

data Literal
    = LInt    Int
    | LFloat  Double
    | LString Text
    | LChar   Char
    | LBool   Bool
    deriving (Show, Eq)

-- ─── OPERADORES ──────────────────────────────────────────────────────────────

data BinOp
    -- Aritméticos
    = OpAdd | OpSub | OpMul | OpDiv | OpMod | OpPow
    -- Comparación
    | OpEq | OpNeq | OpLt | OpGt | OpLte | OpGte
    -- Lógicos
    | OpAnd | OpOr
    -- String/Lista
    | OpConcat  -- ++
    | OpCons    -- :
    -- Función
    | OpComp    -- .
    | OpApply   -- $
    -- IO
    | OpBind    -- >>=
    | OpThen    -- >>
    deriving (Show, Eq)

data UnOp
    = OpNeg     -- -x
    | OpNot     -- not x
    deriving (Show, Eq)
