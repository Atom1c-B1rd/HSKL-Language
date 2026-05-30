module Lexer.Lexer where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Lexer.Tokens

type Parser = Parsec Void Text

-- ─── ESPACIOS Y COMENTARIOS ──────────────────────────────────────────────────

-- | Consume espacios, tabs y comentarios (no newlines, son significativos)
sc :: Parser ()
sc = L.space
    (void $ some (char ' ' <|> char '\t'))
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

-- | Consume espacios incluyendo newlines (para contextos donde no importa)
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | Wrappea un parser consumiendo espacios después
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parsea un símbolo exacto
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- ─── INDENTACIÓN ─────────────────────────────────────────────────────────────

-- | Obtiene el nivel de indentación actual
getIndent :: Parser Pos
getIndent = do
    sp <- getSourcePos
    return $ Pos (unPos $ sourceLine sp) (unPos $ sourceColumn sp)

-- ─── TEMPLATE DELIMITERS ─────────────────────────────────────────────────────

-- | <?hs
openCode :: Parser Token
openCode = TOpenCode <$ symbol "<?hs"

-- | <?=
openInterp :: Parser Token
openInterp = TOpenInterp <$ symbol "<?="

-- | ?>
closeTag :: Parser Token
closeTag = TCloseCode <$ symbol "?>"



-- | Contenido HTML literal (todo lo que no es <?hs o <?=)
htmlContent :: Parser Token
htmlContent = do
    content <- some $ do
        notFollowedBy (string "<?")
        anySingle
    return $ THtml (T.pack content)

openComp :: Parser Token
openComp = do
    void $ char '<'
    notFollowedBy (char '/')   -- que no sea </
    first <- upperChar         -- mayúscula = componente
    rest  <- many alphaNumChar
    void $ char '>'
    return $ TOpenComp (T.pack (first:rest))

closeComp :: Parser Token
closeComp = do
    void $ string "</"
    first <- upperChar
    rest  <- many alphaNumChar
    void $ char '>'
    return $ TCloseComp (T.pack (first:rest))

-- ─── FLAGS / ANOTACIONES ─────────────────────────────────────────────────────

parseFlag :: Parser Token
parseFlag = do
    void $ char '@'
    flag <- choice
        [ FClient  <$ string "client"
        , FServer  <$ string "server"
        , FState   <$ string "state"
        , FPublic  <$ string "public"
        , FPrivate <$ string "private"
        ]
    return $ TAt flag

-- ─── KEYWORDS ────────────────────────────────────────────────────────────────

-- | Parsea una keyword asegurandose que no sea prefijo de un identificador
keyword :: Text -> Token -> Parser Token
keyword kw tok = lexeme $ try $ do
    void $ string kw
    notFollowedBy alphaNumChar
    return tok

keywords :: Parser Token
keywords = choice
    -- Keywords Haskell
    [ keyword "if"     TIf
    , keyword "then"   TThen
    , keyword "else"   TElse
    , keyword "let"    TLet
    , keyword "in"     TIn
    , keyword "where"  TWhere
    , keyword "case"   TCase
    , keyword "of"     TOf
    , keyword "do"     TDo
    -- Keywords HSKL
    , keyword "data"   TData
    , keyword "class"  TClass
    , keyword "impl"   TImpl
    , keyword "import" TImport
    , keyword "module" TModule
    -- Operadores keyword
    , keyword "mod"    TMod
    , keyword "div"    TDiv
    , keyword "not"    TNot
    -- Booleanos
    , keyword "True"   (TBool True)
    , keyword "False"  (TBool False)
    -- Tipos builtin
    , keyword "String"   TTypeString
    , keyword "Int"      TTypeInt
    , keyword "Float"    TTypeFloat
    , keyword "Bool"     TTypeBool
    , keyword "Char"     TTypeChar
    , keyword "Maybe"    TTypeMaybe
    , keyword "Either"   TTypeEither
    , keyword "Map"      TTypeMap
    , keyword "IO"       TTypeIO
    , keyword "Request"  TTypeRequest
    , keyword "Response" TTypeResponse
    , keyword "Ref"      TTypeRef
    , keyword "Void"     TTypeVoid
    ]

-- ─── IDENTIFICADORES ─────────────────────────────────────────────────────────

-- | Identificador normal: empieza con minúscula
parseIdent :: Parser Token
parseIdent = lexeme $ try $ do
    first <- lowerChar
    rest  <- many (alphaNumChar <|> char '_' <|> char '\'')
    return $ TIdent (T.pack (first : rest))

-- | Constructor o Tipo: empieza con mayúscula
parseConstr :: Parser Token
parseConstr = lexeme $ try $ do
    first <- upperChar
    rest  <- many (alphaNumChar <|> char '_' <|> char '\'')
    return $ TConstr (T.pack (first : rest))

-- ─── LITERALES ───────────────────────────────────────────────────────────────

parseInt :: Parser Token
parseInt = lexeme $ TInt . fromInteger <$> L.decimal

parseFloat :: Parser Token
parseFloat = lexeme $ TFloat <$> L.float

parseString :: Parser Token
parseString = lexeme $ do
    void $ char '"'
    content <- many $ choice
        [ char '\\' >> anySingle  -- escape
        , anySingleBut '"'
        ]
    void $ char '"'
    return $ TString (T.pack content)

parseChar :: Parser Token
parseChar = lexeme $ do
    void $ char '\''
    c <- choice
        [ char '\\' >> anySingle
        , anySingleBut '\''
        ]
    void $ char '\''
    return $ TChar c

parseUnit :: Parser Token
parseUnit = TUnit <$ try (symbol "()")

-- ─── OPERADORES ──────────────────────────────────────────────────────────────

operators :: Parser Token
operators = choice
    -- Primero los de múltiples chars (orden importa!)
    [ TDoubleColon <$ symbol "::"
    , TArrow       <$ symbol "->"
    , TFatArrow    <$ symbol "=>"
    , TEqEq        <$ symbol "=="
    , TNotEq       <$ symbol "/="
    , TLtEq        <$ symbol "<="
    , TGtEq        <$ symbol ">="
    , TAnd         <$ symbol "&&"
    , TOr          <$ symbol "||"
    , TFmap        <$ symbol "<$>"
    , TAp          <$ symbol "<*>"
    , TBind        <$ symbol ">>="
    , TThen'       <$ symbol ">>"
    , TConcat      <$ symbol "++"
    , TDotDot      <$ symbol ".."
    -- Después los de un char
    , TEquals      <$ symbol "="
    , TPlus        <$ symbol "+"
    , TMinus       <$ symbol "-"
    , TStar        <$ symbol "*"
    , TSlash       <$ symbol "/"
    , TCaret       <$ symbol "^"
    , TLt          <$ symbol "<"
    , TGt          <$ symbol ">"
    , TDot         <$ symbol "."
    , TDollar      <$ symbol "$"
    , TBackslash   <$ symbol "\\"
    , TCons        <$ symbol ":"
    , TPipe        <$ symbol "|"
    -- Agrupacion
    , TLParen      <$ symbol "("
    , TRParen      <$ symbol ")"
    , TLBracket    <$ symbol "["
    , TRBracket    <$ symbol "]"
    , TLBrace      <$ symbol "{"
    , TRBrace      <$ symbol "}"
    -- Separadores
    , TComma       <$ symbol ","
    , TSemicolon   <$ symbol ";"
    , TUnderscore  <$ symbol "_"
    ]

-- ─── TOKEN PRINCIPAL ─────────────────────────────────────────────────────────

-- | Un token dentro de un bloque de código <?hs ... ?>
codeToken :: Parser Token
codeToken = choice
    [ parseFlag
    , keywords
    , try parseFloat   -- float antes que int
    , parseInt
    , parseString
    , parseChar
    , parseUnit        -- () antes que TLParen
    , parseConstr
    , parseIdent
    , try openComp    -- <-- agregar, ANTES de operators
    , try closeComp 
    , operators
    ]

-- | Tokeniza un bloque completo <?hs ... ?>
codeBlock :: Parser [Token]
codeBlock = do
    void openCode
    tokens <- many (scn *> codeToken)
    void closeTag
    return tokens

-- | Tokeniza una interpolación <?= expr ?>
interpBlock :: Parser [Token]
interpBlock = do
    void openInterp
    tokens <- many (sc *> codeToken)
    void closeTag
    return tokens

-- ─── LEXER PRINCIPAL ─────────────────────────────────────────────────────────

-- | Tokeniza un archivo HSKL completo
tokenize :: Parser [Token]
tokenize = do
    tokens <- many $ choice
        [ codeBlock
        , interpBlock
        , (: []) <$> htmlContent
        ]
    void eof
    return $ concat tokens
