module AtomHtml.Parser(parseHtml, HtmlParseError) where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T 
import Text.Megaparsec 
import Text.Megaparsec.Char

import AtomHtml.Types

type Parser= Parsec Void String
type HtmlParseError = ParseErrorBundle String Void

parseHtml :: String -> Either HtmlParseError [HtmlNode]
parseHtml input = parse (space *> many node <* space <* eof) "<html>" input

node :: Parser HtmlNode
node = dynExpr <|> element <|> textNode

dynExpr :: Parser HtmlNode
dynExpr = do
  _    <- string "<?="
  expr <- manyTill anySingle (string "?>")
  return $ HtmlExpr (trim expr)
 
trim :: String -> String
trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

element :: Parser HtmlNode
element = do
  _        <- char '<'
  tag      <- tagName
  attrs    <- many (try attribute)
  _        <- space
  selfClose <- optional (char '/')
  _        <- char '>'
  case selfClose of
    Just _  -> return $ HtmlElement tag attrs []
    Nothing -> do
      children <- many (notFollowedBy (closeTag tag) *> node)
      _        <- closeTag tag
      return $ HtmlElement tag attrs children
 
closeTag :: Text -> Parser ()
closeTag tag = do
  _ <- string "</"
  _ <- string (T.unpack tag)
  _ <- space
  _ <- char '>'
  return ()
 
tagName :: Parser Text
tagName = T.pack <$> some (alphaNumChar <|> char '-')

attribute :: Parser Attr
attribute = do
  _    <- space1
  name <- T.pack <$> some (alphaNumChar <|> char '-' <|> char ':')
  _    <- char '='
  val  <- quotedValue
  return $ Attr name val
 
quotedValue :: Parser Text
quotedValue = doubleQuoted <|> singleQuoted
  where
    doubleQuoted = T.pack <$> (char '"' *> manyTill anySingle (char '"'))
    singleQuoted = T.pack <$> (char '\'' *> manyTill anySingle (char '\''))

textNode :: Parser HtmlNode
textNode = do
  t <- some (notFollowedBy (char '<') *> anySingle)
  return $ HtmlText (T.pack t)