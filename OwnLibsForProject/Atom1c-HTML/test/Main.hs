{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text)
import qualified Data.Text.IO as TIO

import AtomHtml
suma :: Int -> Int -> Int
suma a b = a + b

saludo :: String -> Text
saludo nombre = "Hola, " <> toHtml nombre

paginaSimple :: Text
paginaSimple = [html|
  <html>
    <head>
      <title>Prueba</title>
    </head>
    <body>
      <h1>Bienvenido</h1>
      <p>Esto es texto estatico</p>
    </body>
  </html>
|]

paginaConExpresiones :: Int -> Int -> Text
paginaConExpresiones a b = [html|
  <html>
    <body>
      <h1>Calculadora</h1>
      <p>Resultado: <?= showIt $ suma a b ?></p>
      <p><?= saludo "mundo" ?></p>
    </body>
  </html>
|]

main :: IO ()
main = do
  putStrLn "=== Pagina estatica ==="
  TIO.putStrLn paginaSimple

  putStrLn "\n=== Pagina con expresiones ==="
  TIO.putStrLn (paginaConExpresiones 3 4)