{-# LANGUAGE OverloadedStrings #-}
module AtomHtml.Render where

import Data.Text (Text)
import qualified Data.Text as T 

import AtomHtml.Types

renderNodes :: [Text] -> Text
renderNodes = T.concat

renderNode :: HtmlNode -> Text
renderNode (HtmlText t)                    = t
renderNode (HtmlExpr _)                    = T.empty
renderNode (HtmlElement tag attrs children) =
  T.concat
    [ "<"
    , tag
    , renderAttrs attrs
    , ">"
    , T.concat (map renderNode children)
    , "</"
    , tag
    , ">"
    ]

 
renderAttrs :: [Attr] -> Text
renderAttrs [] = T.empty
renderAttrs as = T.concat (map renderAttr as)
  where
    renderAttr (Attr name val) =
      T.concat [" ", name, "=\"", escapeHtml val, "\""]


escapeHtml :: Text -> Text
escapeHtml = T.concatMap escape
  where
    escape '&'  = "&amp;"
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '"'  = "&quot;"
    escape '\'' = "&#39;"
    escape c    = T.singleton c

class ToHtml a where
  toHtml :: a -> Text

instance ToHtml Text where
  toHtml = escapeHtml

instance ToHtml String where
  toHtml = escapeHtml . T.pack

instance ToHtml Int where
  toHtml = T.pack . show

instance ToHtml Integer where
  toHtml = T.pack . show

instance ToHtml Double where
  toHtml = T.pack . show

instance ToHtml Bool where
  toHtml = T.pack . show


showIt :: ToHtml a => a -> Text
showIt = toHtml
