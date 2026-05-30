module Server.ServerConfig where

data ServerConfig = ServerConfig
    { configPort   :: Int
    , configRoot   :: FilePath
    , configStatic :: FilePath
    } deriving (Show)

defaultConfig :: ServerConfig
defaultConfig = ServerConfig
    { configPort   = 3000
    , configRoot   = "."
    , configStatic = "."
    }
