{-# LANGUAGE TemplateHaskell #-}

module AtomHtml (html, showIt, toHtml) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse (parseExp)
import Data.Text (Text)
import qualified Data.Text as T

import AtomHtml.Types
import AtomHtml.Parser
import AtomHtml.Render

-- helpers para evitar nombres calificados en TH
concatTexts :: [Text] -> Text
concatTexts = T.concat

packText :: String -> Text
packText = T.pack

html :: QuasiQuoter
html = QuasiQuoter
  { quoteExp  = compileHtml
  , quotePat  = error "html: no se puede usar en patrones"
  , quoteType = error "html: no se puede usar en tipos"
  , quoteDec  = error "html: no se puede usar en declaraciones"
  }

compileHtml :: String -> Q Exp
compileHtml input =
  case parseHtml input of
    Left err    -> fail $ "html-qq error de parseo:\n" ++ show err
    Right nodes -> nodesToExp nodes

nodesToExp :: [HtmlNode] -> Q Exp
nodesToExp nodes = do
  exprs <- mapM nodeToExp nodes
  return $ AppE
    (VarE 'concatTexts)
    (ListE exprs)

nodeToExp :: HtmlNode -> Q Exp
nodeToExp (HtmlText t) =
  return $ AppE
    (VarE 'packText)
    (LitE (StringL (T.unpack t)))

nodeToExp (HtmlExpr exprStr) =
  case parseExp exprStr of
    Left err   -> fail $ "html-qq: expresion invalida en <?= " ++ exprStr ++ " ?>:\n" ++ err
    Right expr -> return expr

nodeToExp (HtmlElement tag attrs children) = do
  childrenExp <- nodesToExp children
  let openTag  = "<" ++ T.unpack tag ++ concatMap buildAttr attrs ++ ">"
      closeTag = "</" ++ T.unpack tag ++ ">"
  return $ AppE
    (VarE 'concatTexts)
    (ListE
      [ strLit openTag
      , childrenExp
      , strLit closeTag
      ])

strLit :: String -> Exp
strLit s = AppE (VarE 'packText) (LitE (StringL s))

buildAttr :: Attr -> String
buildAttr (Attr name val) =
  " " ++ T.unpack name ++ "=\"" ++ T.unpack (escapeHtml val) ++ "\""