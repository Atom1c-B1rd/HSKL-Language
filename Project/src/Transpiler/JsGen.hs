{-# LANGUAGE OverloadedStrings #-}
module Transpiler.JsGen where

import Data.Text (Text)
import qualified Data.Text as T
import Parser.AST (Expr(..), HtmlNode(..), FuncDecl(..), FuncCase(..), DoStmt(..), BinOp(..), UnOp(..), CaseAlt(..), Pat(..), Literal(..), Type(..))
import Lexer.Tokens (Flag(..))

-- ─── ENTRY POINT ─────────────────────────────────────────────────────────────
--
-- Esta es la función principal. Toma una FuncDecl con @client
-- y devuelve el JS como Text.
--
-- HSKL:                          JS:
--   hi :: Void                     function hi() { console.log("hola"); }
--   hi = consoleLog "hola"
--
--   suma :: Int -> Int -> Int       function suma(a) { return (b) => a + b; }
--   suma a b = a + b

transpileDecl :: FuncDecl -> Text
transpileDecl fd =
    let name  = funcName fd
        cases = funcCases fd
    in case cases of
        -- caso simple: una sola ecuación sin patrones complejos
        [FuncCase args body] ->
            case (fmap returnType (funcType fd), body) of
                (Just TyHtml, EHtml nodes) ->
                    -- devuelve HTML, va al servidor
                    name <> " = " <> reconstructHtml nodes <> "\n"
                _ ->
                    -- JS normal
                    let argNames = map patToJs args
                    in "function " <> name
                       <> "(" <> T.intercalate ", " argNames <> ") {\n"
                       <> "  return " <> transpileExpr body <> ";\n"
                       <> "}\n"
        -- múltiples casos: genera if/else por pattern matching
        _ ->
            "function " <> name <> "(...__args) {\n"
            <> T.concat (map (transpileFuncCase name) cases)
            <> "  throw new Error('pattern match failed: " <> name <> "');\n"
            <> "}\n"

transpileFuncCase :: Text -> FuncCase -> Text
transpileFuncCase _ (FuncCase pats body) =
    "  if (" <> T.intercalate " && " (zipWith patCondition ["__args[" <> T.pack (show i) <> "]" | i <- [0..]] pats) <> ") {\n"
    <> "    " <> patBindings pats <> "\n"
    <> "    return " <> transpileExpr body <> ";\n"
    <> "  }\n"

patCondition :: Text -> Pat -> Text
patCondition arg PWild       = "true"
patCondition arg (PVar _)    = "true"
patCondition arg (PLit lit)  = arg <> " === " <> transpileLit lit
patCondition arg (PCon n _)  = arg <> "?.tag === \"" <> n <> "\""
patCondition arg _           = "true"

patBindings :: [Pat] -> Text
patBindings pats = T.concat $ zipWith bind ["__args[" <> T.pack (show i) <> "]" | i <- [0..]] pats
  where
    bind arg (PVar n)     = "const " <> n <> " = " <> arg <> "; "
    bind arg (PCon _ sub) = T.concat $ zipWith (\i p -> bind (arg <> ".fields[" <> T.pack (show i) <> "]") p) [0..] sub
    bind _   _            = ""

patToJs :: Pat -> Text
patToJs (PVar n) = n
patToJs _        = "__p"

returnType :: Type -> Type
returnType (TyFun _ r) = returnType r
returnType t           = t

reconstructHtml :: [HtmlNode] -> Text
reconstructHtml = T.concat . map go
  where
    go (HtmlRaw t)       = t
    go (HtmlExpr e)      = "<?= " <> transpileExpr e <> " ?>"
    go (HtmlComp tag cs) = "<" <> tag <> ">"
                        <> reconstructHtml cs
                        <> "</" <> tag <> ">"

-- ─── EXPRESIONES ─────────────────────────────────────────────────────────────
--
-- El patrón es siempre: mirás el nodo del AST y generás el JS equivalente.
-- Es igual que eval pero sin IO y devolviendo Text.

transpileExpr :: Expr -> Text

-- Literales: traducción directa
transpileExpr (EInt    n) = T.pack (show n)
transpileExpr (EFloat  f) = T.pack (show f)
transpileExpr (EString s) = "\"" <> escapeJs s <> "\""
transpileExpr (EChar   c) = "\"" <> T.singleton c <> "\""
transpileExpr (EBool   True)  = "true"
transpileExpr (EBool   False) = "false"
transpileExpr EUnit           = "undefined"

-- Variables y constructores: pasan directo
transpileExpr (EVar name) = name
transpileExpr (ECon name) = name

-- Aplicación de función:
-- En HSKL: f x y  es  EApp (EApp f x) y
-- En JS queremos: f(x, y)  no  f(x)(y)
-- Por eso aplanamos primero con flattenApp
transpileExpr expr@(EApp _ _) =
    let (func, args) = flattenApp expr
    in transpileExpr func <> "(" <> T.intercalate ", " (map transpileExpr args) <> ")"

-- Lambda: \x y -> expr  →  (x, y) => expr
transpileExpr (ELam args body) =
    "(" <> T.intercalate ", " args <> ") => "
    <> transpileExpr body

-- Let: let x = e1 in e2
-- En JS lo convertimos en una IIFE (función inmediatamente invocada)
-- para crear el scope local:  (() => { const x = e1; return e2; })()
transpileExpr (ELet bindings body) =
    "(() => {\n"
    <> T.concat (map transpileBinding bindings)
    <> "  return " <> transpileExpr body <> ";\n"
    <> "})()"

-- If: if c then a else b  →  (c ? a : b)
-- Usamos ternario de JS, con paréntesis para evitar precedencia rara
transpileExpr (EIf cond true false) =
    "(" <> transpileExpr cond
    <> " ? " <> transpileExpr true
    <> " : " <> transpileExpr false <> ")"

-- Case: lo convertimos en if/else if encadenados
-- Es una simplificación, pattern matching complejo no se soporta aún
transpileExpr (ECase scrutinee alts) =
    transpileCase scrutinee alts

-- Do notation: secuencia de statements
-- Cada statement se convierte en una línea de JS
transpileExpr (EDo stmts) =
    "(() => {\n"
    <> T.concat (map transpileDoStmt stmts)
    <> "})()"

-- Ref: ref x  →  { current: x }
-- Simulamos IORef con un objeto en JS
transpileExpr (ERef expr) =
    "{ current: " <> transpileExpr expr <> " }"

-- Operadores binarios
transpileExpr (EBinOp op l r) = transpileBinOp op l r

-- Operadores unarios
transpileExpr (EUnOp OpNeg expr) = "(-" <> transpileExpr expr <> ")"
transpileExpr (EUnOp OpNot expr) = "(!" <> transpileExpr expr <> ")"

-- Html: template literal
transpileExpr (EHtml nodes) =
    "`" <> T.concat (map transpileNode nodes) <> "`"

-- Lista: [1, 2, 3]  →  [1, 2, 3]  (igual en JS!)
transpileExpr (EList exprs) =
    "[" <> T.intercalate ", " (map transpileExpr exprs) <> "]"

-- Tupla: (a, b)  →  [a, b]  (JS no tiene tuplas, usamos array)
transpileExpr (ETuple exprs) =
    "[" <> T.intercalate ", " (map transpileExpr exprs) <> "]"

-- Where: igual que let pero al revés en el AST
transpileExpr (EWhere body bindings) =
    "(() => {\n"
    <> T.concat (map transpileBinding bindings)
    <> "  return " <> transpileExpr body <> ";\n"
    <> "})()"

-- ─── OPERADORES BINARIOS ─────────────────────────────────────────────────────
--
-- La mayoría son 1 a 1. Los especiales:
--   ++  →  +    (concatenación en HSKL, suma/concat en JS)
--   :   →  .concat  (cons de lista)
--   .   →  no existe directo, usamos función wrapper
--   $   →  desaparece, es solo aplicación

transpileBinOp :: BinOp -> Expr -> Expr -> Text
transpileBinOp op l r =
    let ljs = transpileExpr l
        rjs = transpileExpr r
        infix' op = "(" <> ljs <> " " <> op <> " " <> rjs <> ")"
    in case op of
        -- Aritméticos
        OpAdd    -> infix' "+"
        OpSub    -> infix' "-"
        OpMul    -> infix' "*"
        OpDiv    -> infix' "/"
        OpMod    -> infix' "%"
        OpPow    -> infix' "**"
        -- Comparación
        OpEq     -> infix' "==="
        OpNeq    -> infix' "!=="
        OpLt     -> infix' "<"
        OpGt     -> infix' ">"
        OpLte    -> infix' "<="
        OpGte    -> infix' ">="
        -- Lógicos
        OpAnd    -> infix' "&&"
        OpOr     -> infix' "||"
        -- ++ en HSKL es concat de strings o listas
        -- en JS + funciona para strings, concat() para arrays
        -- usamos + que funciona en ambos casos básicos
        OpConcat -> infix' "+"
        -- : (cons) →  [l, ...r]  spread del resto
        OpCons   -> "[" <> ljs <> ", ..." <> rjs <> "]"
        -- $ desaparece, es solo f $ x = f(x)
        OpApply  -> transpileExpr l <> "(" <> transpileExpr r <> ")"
        -- . composición →  (x) => f(g(x))
        OpComp   -> "(x) => " <> ljs <> "(" <> rjs <> "(x))"
        -- >>= y >> para IO/Promise
        OpBind   -> ljs <> ".then(" <> rjs <> ")"
        OpThen   -> ljs <> ".then(() => " <> rjs <> ")"

-- ─── HELPERS ─────────────────────────────────────────────────────────────────

-- | Aplana una cadena de aplicaciones
-- EApp (EApp (EApp f x) y) z  →  (f, [x, y, z])
-- Así podemos generar f(x, y, z) en vez de f(x)(y)(z)
flattenApp :: Expr -> (Expr, [Expr])
flattenApp (EApp f x) =
    let (func, args) = flattenApp f
    in  (func, args ++ [x])
flattenApp e = (e, [])

-- | Transpila un binding de let/where
transpileBinding :: (Text, Expr) -> Text
transpileBinding (name, expr) =
    "  const " <> name <> " = " <> transpileExpr expr <> ";\n"

-- | Transpila un statement de do notation
transpileDoStmt :: DoStmt -> Text
transpileDoStmt (DoBind name expr) =
    "  const " <> name <> " = await " <> transpileExpr expr <> ";\n"
transpileDoStmt (DoExpr expr) =
    "  " <> transpileExpr expr <> ";\n"
transpileDoStmt (DoLet name expr) =
    "  const " <> name <> " = " <> transpileExpr expr <> ";\n"

-- | Transpila case como if/else if
-- Solo soporta patrones simples por ahora
transpileCase :: Expr -> [CaseAlt] -> Text
transpileCase scrutinee [] = "undefined"
transpileCase scrutinee alts =
    let scrJs = transpileExpr scrutinee
        conditions = map (transpileAlt scrJs) alts
    in T.intercalate " else " conditions

transpileAlt :: Text -> CaseAlt -> Text
transpileAlt scrJs (CaseAlt pat body) =
    case pat of
        PWild       -> "{ return " <> transpileExpr body <> "; }"
        PVar name   -> "{ const " <> name <> " = " <> scrJs
                       <> "; return " <> transpileExpr body <> "; }"
        PLit lit    -> "if (" <> scrJs <> " === " <> transpileLit lit <> ") "
                       <> "{ return " <> transpileExpr body <> "; }"
        PCon name _ -> "if (" <> scrJs <> "?.tag === \"" <> name <> "\") "
                       <> "{ return " <> transpileExpr body <> "; }"
        _           -> "{ return " <> transpileExpr body <> "; }"

transpileLit :: Literal -> Text
transpileLit (LInt    n) = T.pack (show n)
transpileLit (LFloat  f) = T.pack (show f)
transpileLit (LString s) = "\"" <> s <> "\""
transpileLit (LChar   c) = "\"" <> T.singleton c <> "\""
transpileLit (LBool True)  = "true"
transpileLit (LBool False) = "false"

-- | Escapa caracteres especiales en strings JS
escapeJs :: Text -> Text
escapeJs = T.concatMap escape
  where
    escape '"'  = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape c    = T.singleton c

-- ─── GENERADOR DEL BLOQUE SCRIPT ─────────────────────────────────────────────
--
-- Esta es la función que llama Runner.hs al final de procesar el archivo.
-- Toma todas las FuncDecl con @client y genera un bloque <script>.

generateScript :: [FuncDecl] -> Text
generateScript [] = ""  -- sin @client, sin script
generateScript fds =
    "\n<script>\n"
    -- Primero los helpers de runtime que siempre necesitamos
    <> runtimeHelpers
    <> "\n"
    -- Después las funciones del usuario
    <> T.intercalate "\n" (map transpileDecl fds)
    <> "</script>\n"

-- | Funciones helper que siempre se incluyen cuando hay @client
-- Son el "runtime" mínimo de HSKL en el browser
runtimeHelpers :: Text
runtimeHelpers = T.unlines
    [ "// HSKL Runtime"
    -- Ya existentes
    , "const consoleLog = (x) => { console.log(x); };"
    , "const show = (x) => String(x);"
    -- DOM - obtener valores
    , "const getValue = (id) => document.getElementById(id)?.value ?? '';"
    , "const getText  = (id) => document.getElementById(id)?.textContent ?? '';"
    -- DOM - setear valores  
    , "const setValue = (id) => (val) => { document.getElementById(id).value = val; };"
    , "const setText  = (id) => (val) => { document.getElementById(id).textContent = val; };"
    , "const setHtml  = (id) => (val) => { document.getElementById(id).innerHTML = val; };"
    -- DOM - eventos
    , "const preventDefault = (e) => { e.preventDefault(); };"
    , "const stopPropagation = (e) => { e.stopPropagation(); };"
    -- DOM - clases CSS
    , "const addClass    = (id) => (cls) => document.getElementById(id).classList.add(cls);"
    , "const removeClass = (id) => (cls) => document.getElementById(id).classList.remove(cls);"
    , "const toggleClass = (id) => (cls) => document.getElementById(id).classList.toggle(cls);"
    -- Fetch al servidor (para después)
    , "const fetchGet  = (url) => fetch(url).then(r => r.json());"
    , "const fetchPost = (url) => (body) => fetch(url, {method:'POST', body: JSON.stringify(body)}).then(r => r.json());"
    ]

-- ─── HTML ────────────────────────────────────────────────────────────────────

transpileNode :: HtmlNode -> Text
transpileNode (HtmlRaw  t)       = t
transpileNode (HtmlExpr e)       = "${" <> transpileExpr e <> "}"
transpileNode (HtmlComp _ nodes) = T.concat (map transpileNode nodes)