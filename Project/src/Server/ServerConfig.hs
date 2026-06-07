module Server.ServerConfig where

data ServerConfig = ServerConfig
    { configPort   :: Int
    , configRoot   :: FilePath
    , configStatic :: FilePath
    } deriving (Show)

defaultConfig :: ServerConfig
defaultConfig = ServerConfig
    { configPort   = 8080
    , configRoot   = "."
    , configStatic = "."
    }
