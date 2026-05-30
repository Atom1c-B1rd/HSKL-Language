{-# LANGUAGE OverloadedStrings #-}
module Parser.Parser where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Lexer.Tokens (Flag(..))
import Parser.AST

-- ─── SETUP ───────────────────────────────────────────────────────────────────

type Parser = Parsec Void Text

-- | Espacios y comentarios (sin newlines, son significativos)
sc :: Parser ()
sc = L.space
    (void $ takeWhile1P Nothing (\c -> c == ' ' || c == '\t'))
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

-- | Espacios incluyendo newlines
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- ─── HELPERS BÁSICOS ─────────────────────────────────────────────────────────

-- EJEMPLO: así se parsea una keyword protegida
-- La clave es el `notFollowedBy alphaNumChar`
-- sin eso "letra" matchearía con "let"
keyword :: Text -> Parser Text
keyword kw = lexeme $ try $ do
    s <- string kw
    notFollowedBy alphaNumChar  -- "letX" no es keyword "let"
    return s

-- EJEMPLO: identificador en minúscula (variables, funciones)
-- `lowerChar` parsea una letra minúscula
-- `many` parsea cero o más del siguiente
parseIdent :: Parser Text
parseIdent = lexeme $ try $ do
    first <- lowerChar
    rest  <- many (alphaNumChar <|> char '_' <|> char '\'')
    let ident = T.pack (first : rest)
    -- verificamos que no sea una keyword
    if ident `elem` reservedWords
        then fail $ "keyword reservada: " ++ T.unpack ident
        else return ident

-- EJEMPLO: constructor en mayúscula (tipos, constructores de data)
parseConstr :: Parser Text
parseConstr = lexeme $ try $ do
    first <- upperChar
    rest  <- many (alphaNumChar <|> char '_')
    return $ T.pack (first : rest)

reservedWords :: [Text]
reservedWords =
    [ "if", "then", "else", "let", "in", "where"
    , "case", "of", "do", "data", "class", "impl"
    , "import", "module", "mod", "div", "not"
    , "True", "False"
    ]

-- ─── FLAGS ───────────────────────────────────────────────────────────────────

-- EJEMPLO: `optional` intenta el parser, devuelve Nothing si falla
-- muy útil para cosas opcionales como los flags
parseFlag :: Parser (Maybe Flag)
parseFlag = optional $ do
    void $ char '@'
    choice
        [ FClient  <$ keyword "client"
        , FServer  <$ keyword "server"
        , FState   <$ keyword "state"
        , FPublic  <$ keyword "public"
        , FPrivate <$ keyword "private"
        ]

-- ─── TIPOS ───────────────────────────────────────────────────────────────────

-- EJEMPLO: parsear tipos con precedencia
-- TyFun tiene la menor precedencia (->)
-- así "String -> Int -> Bool" parsea como "String -> (Int -> Bool)"
parseType :: Parser Type
parseType = do
    t <- parseTypeApp             -- parsea el lado izquierdo
    option t $ do                 -- `option` devuelve t si lo siguiente falla
        void $ symbol "->"
        TyFun t <$> parseType    -- recursivo para asociar a la derecha

-- | Aplicación de tipos: "Maybe Int", "IO String"
-- tiene más precedencia que ->
parseTypeApp :: Parser Type
parseTypeApp = do
    ts <- some parseTypeAtom     -- uno o más tipos atómicos
    -- foldl1 construye la aplicación: Maybe Int = TyApp (TyCon "Maybe") (TyCon "Int")
    return $ foldl1 TyApp ts

-- | Tipo atómico: el más básico, sin aplicación ni ->
parseTypeAtom :: Parser Type
parseTypeAtom = choice
    [ TyUnit <$ try (symbol "()")
    -- EJEMPLO: between parsea algo entre dos delimitadores
    , TyTuple <$> between (symbol "(") (symbol ")") (parseType `sepBy1` symbol ",")
    , TyList  <$> between (symbol "[") (symbol "]") parseType
    -- paréntesis solos para agrupar: (String -> Int)
    , between (symbol "(") (symbol ")") parseType
    , TyCon <$> parseConstr
    ]

-- ─── LITERALES ───────────────────────────────────────────────────────────────

parseLiteral :: Parser Expr
parseLiteral = choice
    [ EFloat  <$> try (lexeme L.float)   -- float antes que int!
    , EInt    <$> lexeme (fromInteger <$> L.decimal)
    , EString <$> parseStringLit
    , EChar   <$> parseCharLit
    , EBool   True  <$ keyword "True"
    , EBool   False <$ keyword "False"
    , EUnit   <$  try (symbol "()")
    ]

parseStringLit :: Parser Text
parseStringLit = lexeme $ do
    void $ char '"'
    chars <- many $ choice
        [ char '\\' >> escapeChar
        , anySingleBut '"'
        ]
    void $ char '"'
    return $ T.pack chars
  where
    escapeChar = choice
        [ '\n' <$ char 'n'
        , '\t' <$ char 't'
        , '\\' <$ char '\\'
        , '"'  <$ char '"'
        ]

parseCharLit :: Parser Char
parseCharLit = lexeme $ between (char '\'') (char '\'') anySingle

-- ─── EXPRESIONES ─────────────────────────────────────────────────────────────

-- EJEMPLO: parsear con precedencia de operadores
-- La técnica es separar en niveles, cada nivel llama al siguiente
-- Nivel más bajo (menor precedencia) llama al más alto
--
-- Precedencia en HSKL (de menor a mayor):
-- 1. >>= >>          (bind IO)
-- 2. $               (aplicación derecha)
-- 3. && ||           (lógicos)
-- 4. == /= < > <= >= (comparación)
-- 5. ++ :            (listas)
-- 6. + -             (suma)
-- 7. * / mod div     (multiplicación)
-- 8. .               (composición)
-- 9. f x y           (aplicación de función, mayor precedencia)
-- 10. átomos

parseExpr :: Parser Expr
parseExpr = parseBindExpr

-- Nivel 1: >>= y >>
parseBindExpr :: Parser Expr
parseBindExpr = do
    e <- parseApplyDollar
    option e $ choice
        [ EBinOp OpBind e <$ symbol ">>=" <*> parseBindExpr
        , EBinOp OpThen e <$ symbol ">>"  <*> parseBindExpr
        ]

-- Nivel 2: $ (asocia a la derecha)
parseApplyDollar :: Parser Expr
parseApplyDollar = do
    e <- parseLogical
    option e $ EBinOp OpApply e <$ symbol "$" <*> parseApplyDollar

-- Nivel 3: && y ||
parseLogical :: Parser Expr
parseLogical = do
    e <- parseComparison
    option e $ choice
        [ EBinOp OpAnd e <$ symbol "&&" <*> parseLogical
        , EBinOp OpOr  e <$ symbol "||" <*> parseLogical
        ]

-- Nivel 4: ==, /=, <, >, <=, >=
parseComparison :: Parser Expr
parseComparison = do
    e <- parseConcat
    option e $ choice
        [ EBinOp OpEq  e <$ symbol "==" <*> parseConcat
        , EBinOp OpNeq e <$ symbol "/=" <*> parseConcat
        , EBinOp OpLte e <$ try (symbol "<=") <*> parseConcat
        , EBinOp OpGte e <$ try (symbol ">=") <*> parseConcat
        , EBinOp OpLt  e <$ symbol "<"  <*> parseConcat
        , EBinOp OpGt  e <$ symbol ">"  <*> parseConcat
        ]

-- Nivel 5: ++ y :
parseConcat :: Parser Expr
parseConcat = do
    e <- parseAddSub
    option e $ choice
        [ EBinOp OpConcat e <$ try (symbol "++") <*> parseConcat
        , EBinOp OpCons   e <$ try (symbol ":" <* notFollowedBy (char ':')) <*> parseConcat
        ]

-- Nivel 6: + y -
parseAddSub :: Parser Expr
parseAddSub = do
    e <- parseMulDiv
    rest <- many $ choice
        [ (OpAdd,) <$ try (symbol "+" <* notFollowedBy (char '+')) <*> parseMulDiv
        , (OpSub,) <$ try (symbol "-" <* notFollowedBy (char '>')) <*> parseMulDiv
        ]
    return $ foldl (\acc (op, r) -> EBinOp op acc r) e rest

-- Nivel 7: * / mod div
parseMulDiv :: Parser Expr
parseMulDiv = do
    e <- parseCompose
    rest <- many $ choice
        [ (OpMul,) <$ symbol "*"        <*> parseCompose
        , (OpDiv,) <$ symbol "/"        <*> parseCompose
        , (OpMod,) <$ keyword "mod"     <*> parseCompose
        , (OpMod,) <$ keyword "div"     <*> parseCompose
        ]
    return $ foldl (\acc (op, r) -> EBinOp op acc r) e rest

-- Nivel 8: . (composición, asocia a la derecha)
parseCompose :: Parser Expr
parseCompose = do
    e <- parseApp
    option e $ EBinOp OpComp e <$ symbol "." <*> parseCompose

-- Nivel 9: aplicación de función (f x y z)
-- EJEMPLO: `some` para parsear f seguido de uno o más argumentos
parseApp :: Parser Expr
parseApp = do
    f    <- parseAtom
    args <- many parseAtom   -- los argumentos son átomos (no expresiones completas)
    -- foldl construye la aplicación izquierda: f x y = App (App f x) y
    return $ foldl EApp f args

-- Nivel 10: átomos (no se pueden descomponer más sin paréntesis)
parseAtom :: Parser Expr
parseAtom = choice
    -- EJEMPLO: `try` necesario cuando múltiples alternativas comparten prefijo
    [ parseLiteral
    , parseLambda
    , parseLetExpr
    , parseIfExpr
    , parseCaseExpr
    , parseDoExpr
    , ERef <$> (keyword "ref" *> parseAtom)
    , try parseHtmlBlock
    -- EJEMPLO: between para paréntesis
    , parseTupleOrParen
    , EList <$> between (symbol "[") (symbol "]") (parseExpr `sepBy` symbol ",")
    , ECon  <$> parseConstr
    , EVar  <$> parseIdent
    ]

-- | Parsea (expr) o (e1, e2, e3)
-- EJEMPLO: `sepBy1` parsea uno o más separados por comas
parseTupleOrParen :: Parser Expr
parseTupleOrParen = between (symbol "(") (symbol ")") $ do
    e  <- parseExpr
    es <- many (symbol "," *> parseExpr)
    return $ case es of
        [] -> e                  -- solo (expr) → agrupación
        _  -> ETuple (e : es)   -- (e1, e2, ...) → tupla

-- | Lambda: \x y -> expr
parseLambda :: Parser Expr
parseLambda = do
    void $ symbol "\\"
    args <- some parseIdent      -- uno o más argumentos
    void $ symbol "->"
    ELam args <$> parseExpr

-- | Let: let x = e1 in e2
parseLetExpr :: Parser Expr
parseLetExpr = do
    void $ keyword "let"
    -- EJEMPLO: `sepBy1` para múltiples bindings
    bindings <- some $ do
        name <- parseIdent
        void $ symbol "="
        e    <- parseExpr
        scn
        return (name, e)
    void $ keyword "in"
    ELet bindings <$> parseExpr

-- | If: if e1 then e2 else e3
parseIfExpr :: Parser Expr
parseIfExpr = do
    void $ keyword "if"
    cond  <- parseExpr
    void $ keyword "then"
    true  <- parseExpr
    void $ keyword "else"
    EIf cond true <$> parseExpr

-- | Case: case expr of { pat -> expr; ... }
parseCaseExpr :: Parser Expr
parseCaseExpr = do
    void $ keyword "case"
    e    <- parseExpr
    void $ keyword "of"
    -- EJEMPLO: bloque de indentación con megaparsec
    alts <- parseBlock parseCaseAlt
    return $ ECase e alts

parseCaseAlt :: Parser CaseAlt
parseCaseAlt = do
    pat <- parsePat
    void $ symbol "->"
    CaseAlt pat <$> parseExpr

-- | Do notation
parseDoExpr :: Parser Expr
parseDoExpr = do
    void $ keyword "do"
    EDo <$> parseBlock parseDoStmt

parseDoStmt :: Parser DoStmt
parseDoStmt = choice
    [ try $ DoBind <$> parseIdent <* symbol "<-" <*> parseExpr
    , try $ DoLet  <$> (keyword "let" *> parseIdent) <* symbol "=" <*> parseExpr
    , DoExpr <$> parseExpr
    ]

-- ─── PATRONES ────────────────────────────────────────────────────────────────

parsePat :: Parser Pat
parsePat = choice
    [ PWild  <$  symbol "_"
    , PLit   <$> parseLiteralPat
    , try $ PCon <$> parseConstr <*> many parsePatAtom
    , PTuple <$> between (symbol "(") (symbol ")") (parsePat `sepBy1` symbol ",")
    , PList  <$> between (symbol "[") (symbol "]") (parsePat `sepBy`  symbol ",")
    , PVar   <$> parseIdent
    ]

parsePatAtom :: Parser Pat
parsePatAtom = choice
    [ PWild  <$  symbol "_"
    , PLit   <$> parseLiteralPat
    , PCon   <$> parseConstr <*> pure []
    , PVar   <$> parseIdent
    ]

parseLiteralPat :: Parser Literal
parseLiteralPat = choice
    [ LFloat  <$> try (lexeme L.float)
    , LInt    <$> lexeme (fromInteger <$> L.decimal)
    , LString <$> parseStringLit
    , LChar   <$> parseCharLit
    , LBool True  <$ keyword "True"
    , LBool False <$ keyword "False"
    ]

-- ─── DECLARACIONES ───────────────────────────────────────────────────────────

parseDecl :: Parser Decl
parseDecl = choice
    [ DData  <$> parseDataDecl
    , DClass <$> parseClassDecl
    , DImport <$> (keyword "import" *> (T.intercalate "." <$> parseConstr `sepBy1` symbol "."))
    , DFunc  <$> parseFuncDecl
    ]

-- | data Arbol = Hoja | Nodo Int Arbol Arbol
parseDataDecl :: Parser DataDecl
parseDataDecl = do
    void $ keyword "data"
    name <- parseConstr
    void $ symbol "="
    -- EJEMPLO: `sepBy1` con pipe para constructores
    cons <- parseConstructor `sepBy1` symbol "|"
    return $ DataDecl name cons

parseConstructor :: Parser Constructor
parseConstructor = do
    name   <- parseConstr
    fields <- many parseTypeAtom   -- campos son tipos atómicos
    return $ Constructor name fields

-- | class Nombre impl Ejemplo { ... }
parseClassDecl :: Parser ClassDecl
parseClassDecl = do
    void $ keyword "class"
    name    <- parseConstr
    impl    <- optional (keyword "impl" *> parseConstr)
    members <- parseBlock parseClassMember
    return $ ClassDecl name impl members

parseClassMember :: Parser ClassMember
parseClassMember = do
    flag <- parseFlag
    decl <- parseFuncDecl
    return $ ClassMember flag decl

-- | Función con firma de tipo opcional y definición
-- @client          <- flag
-- nombre :: Type   <- firma (opcional)
-- nombre args = expr
parseFuncDecl :: Parser FuncDecl
parseFuncDecl = do
    flag <- parseFlag
    scn
    name <- parseIdent
    -- EJEMPLO: `try` para intentar parsear la firma de tipo
    -- si falla (no hay ::) volvemos y parseamos directo la definición
    sig  <- optional $ try $ do
        void $ symbol "::"
        parseType
    scn
    -- ahora la definición: nombre args = expr
    defName <- parseIdent
    if defName /= name
        then fail $ "esperaba definición de '" ++ T.unpack name ++ "' pero encontré '" ++ T.unpack defName ++ "'"
        else do
            args <- many parseIdent
            void $ symbol "="
            body <- parseExpr
            return $ FuncDecl flag name sig args body

-- ─── BLOQUES DE INDENTACIÓN ──────────────────────────────────────────────────

-- EJEMPLO: así se maneja la indentación con Megaparsec
-- `L.indentBlock` maneja automáticamente el layout
parseBlock :: Parser a -> Parser [a]
parseBlock p = do
    -- opcionalmente abrimos con {
    explicit <- optional (symbol "{")
    case explicit of
        Just _  -> p `sepEndBy` (symbol ";" <|> (T.pack "\n" <$ newline)) <* symbol "}"
        Nothing -> L.indentBlock scn $ do
            first <- p
            return $ L.IndentMany Nothing (return . (first:)) p

-- ─── TEMPLATE (secciones del archivo) ────────────────────────────────────────

parseSection :: Parser Section
parseSection = choice
    [ parseCodeBlock
    , parseInterpBlock
    , parseHtml
    ]

-- | <?hs ... ?>
parseCodeBlock :: Parser Section
parseCodeBlock = do
    void $ string "<?hs"
    scn
    decls <- many (scn *> parseDecl <* scn)
    void $ string "?>"
    return $ SCode decls

-- | <?= expr ?>
parseInterpBlock :: Parser Section
parseInterpBlock = do
    void $ string "<?="
    sc
    e <- parseExpr
    sc
    void $ string "?>"
    return $ SInterp e

-- | HTML literal
parseHtml :: Parser Section
parseHtml = do
    content <- some $ do
        notFollowedBy (string "<?")
        anySingle
    return $ SHtml (T.pack content)

-- ─── PROGRAMA COMPLETO ───────────────────────────────────────────────────────

parseProgram :: Parser Program
parseProgram = Program <$> many parseSection <* eof

-- ─── HTML BLOCKS (tipo Html) ──────────────────────────────────────────────────

parseHtmlBlock :: Parser Expr
parseHtmlBlock = do
    void $ parseOpenComp
    nodes <- many parseHtmlNode
    void $ parseCloseComp
    return $ EHtml nodes

parseHtmlNode :: Parser HtmlNode
parseHtmlNode = choice
    [ HtmlExpr <$> parseInterpInline
    , try $ HtmlComp <$> parseOpenComp <*> many parseHtmlNode <* parseCloseComp
    , HtmlRaw  <$> parseRawHtml
    ]

parseInterpInline :: Parser Expr
parseInterpInline = do
    void $ string "<?="
    sc
    e <- parseExpr
    sc
    void $ string "?>"
    return e

parseRawHtml :: Parser Text
parseRawHtml = do
    chars <- some $ do
        notFollowedBy (void parseOpenComp)
        notFollowedBy (string "<?")
        anySingle
    return $ T.pack chars

parseOpenComp :: Parser Text
parseOpenComp = try $ do
    void $ char '<'
    notFollowedBy (char '/')
    first <- upperChar
    rest  <- many alphaNumChar
    void $ char '>'
    return $ T.pack (first:rest)

parseCloseComp :: Parser Text
parseCloseComp = try $ do
    void $ string "</"
    first <- upperChar
    rest  <- many alphaNumChar
    void $ char '>'
    return $ T.pack (first:rest)