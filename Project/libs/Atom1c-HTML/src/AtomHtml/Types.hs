module AtomHtml.Types where

import Data.Text(Text)
data Attr = Attr
    { attrName :: Text
    , attrValue :: Text
    }deriving (Show, Eq)

data HtmlNode
    = HtmlElement Text [Attr] [HtmlNode]
    | HtmlText Text
    | HtmlExpr String
    deriving(Show,Eq)