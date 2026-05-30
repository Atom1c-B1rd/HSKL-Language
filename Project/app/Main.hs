module Main where

import Options.Applicative
import Server.Server (startServer)
import Server.ServerConfig

data Opts = Opts
    { optPort   :: Int
    , optRoot   :: FilePath
    , optStatic :: FilePath
    }

optsParser :: Parser Opts
optsParser = Opts
    <$> option auto
        ( long "port"   <> short 'p' <> metavar "PORT"
        <> value 3000   <> help "Puerto del servidor (default: 3000)" )
    <*> strOption
        ( long "root"   <> short 'r' <> metavar "DIR"
        <> value "./"  <> help "Directorio raíz .hskl (default: ./pages)" )
    <*> strOption
        ( long "static" <> short 's' <> metavar "DIR"
        <> value "./" <> help "Archivos estáticos (default: ./public)" )

main :: IO ()
main = do
    opts <- execParser $ info (optsParser <**> helper)
        ( fullDesc
        <> progDesc "HSKL - Haskell Script Killing Language"
        <> header   "hskl - un PHP versión Haskell" )
    startServer $ ServerConfig
        { configPort   = optPort opts
        , configRoot   = optRoot opts
        , configStatic = optStatic opts
        }
