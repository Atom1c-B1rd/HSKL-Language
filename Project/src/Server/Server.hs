module Server.Server where

import Web.Scotty
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, dropExtension, takeFileName)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, SomeException)
import Data.List (isPrefixOf, sort)

import Server.Runner
import Server.ServerConfig

-- ─── ENTRY POINT ─────────────────────────────────────────────────────────────

-- | Levanta el servidor en el puerto dado con el directorio raíz
-- Uso:
--   startServer defaultConfig
--   startServer defaultConfig { configPort = 8080, configRoot = "./src/pages" }
startServer :: Server.ServerConfig.ServerConfig -> IO ()
startServer config = do
    routes <- discoverRoutes (configRoot config)
    putStrLn $ "🚀 HSKL corriendo en http://localhost:" ++ show (configPort config)
    putStrLn $ "📁 Sirviendo desde: " ++ configRoot config
    putStrLn $ "📋 Rutas encontradas:"
    mapM_ (\r -> putStrLn $ "   " ++ showRoute r) routes
    putStrLn ""
    scotty (configPort config) $ do
        -- Archivos estáticos primero
        -- middleware $ staticPolicy (addBase (configStatic config))
        -- Rutas dinámicas generadas desde los archivos
        mapM_ (registerRoute config) routes

-- ─── DESCUBRIMIENTO DE RUTAS ─────────────────────────────────────────────────

data Route = Route
    { routePath     :: FilePath     -- path del archivo en disco
    , routePattern  :: String       -- patrón de la ruta HTTP: "/", "/about", etc
    , routeSegments :: [Segment]    -- segmentos parseados (para params dinámicos)
    } deriving (Show)

data Segment
    = Static  String    -- segmento fijo: "about", "blog"
    | Dynamic String    -- segmento dinámico: ":id", ":slug"
    deriving (Show)

showRoute :: Route -> String
showRoute r = "GET " ++ routePattern r ++ "  →  " ++ routePath r

-- | Escanea el directorio y genera rutas a partir de los archivos .hskl
-- Estructura de ejemplo:
--   pages/
--     index.hskl        →  /
--     about.hskl        →  /about
--     blog/
--       index.hskl      →  /blog
--       [slug].hskl     →  /blog/:slug   (dinámico)
--     api/
--       users.hskl      →  /api/users
discoverRoutes :: FilePath -> IO [Route]
discoverRoutes root = do
    exists <- doesDirectoryExist root
    if not exists
        then do
            putStrLn $ "⚠️  Directorio no encontrado: " ++ root
            return []
        else do
            files <- scanDir root root
            return $ sort' $ map (fileToRoute root) files
  where
    sort' = id  -- podríamos ordenar por profundidad si hace falta

-- | Escanea recursivamente buscando archivos .hskl
scanDir :: FilePath -> FilePath -> IO [FilePath]
scanDir root dir = do
    entries <- listDirectory dir
    results <- mapM (processEntry dir) entries
    return $ concat results
  where
    processEntry parent entry = do
        let full = parent </> entry
        isDir  <- doesDirectoryExist full
        isFile <- doesFileExist full
        if isDir
            then scanDir root full
            else if takeExtension entry == ".hskl"
                    then return [full]
                    else return []

-- | Convierte un path de archivo a una Route
-- pages/index.hskl          →  /
-- pages/about.hskl          →  /about
-- pages/blog/index.hskl     →  /blog
-- pages/blog/[slug].hskl    →  /blog/:slug
fileToRoute :: FilePath -> FilePath -> Route
fileToRoute root filePath =
    let relative  = drop (length root + 1) filePath        -- quita el root
        noExt     = dropExtension relative                  -- quita .hskl
        segments  = splitPath noExt                        -- divide por /
        cleaned   = removeIndex segments                   -- index → ""
        pattern   = buildPattern cleaned                   -- construye /path/:param
        segs      = map parseSegment cleaned
    in Route filePath pattern segs
  where
    splitPath = wordsBy (== '/')
    removeIndex segs
        | null segs             = []
        | last segs == "index"  = init segs
        | otherwise             = segs
    buildPattern []   = "/"
    buildPattern segs = "/" ++ joinWith "/" (map segToPattern segs)
    segToPattern s
        | isDynamic s = ":" ++ stripBrackets s
        | otherwise   = s
    parseSegment s
        | isDynamic s = Dynamic (stripBrackets s)
        | otherwise   = Static s
    isDynamic s = "[" `isPrefixOf` s && last s == ']'
    stripBrackets = init . tail
    joinWith _ []     = ""
    joinWith sep xs   = foldr1 (\a b -> a ++ sep ++ b) xs
    wordsBy p s = case dropWhile p s of
        "" -> []
        s' -> let (w, rest) = break p s'
              in  w : wordsBy p rest

-- ─── REGISTRO DE RUTAS EN SCOTTY ─────────────────────────────────────────────

-- | Registra una ruta en Scotty
-- Por ahora solo GET, después agregaremos POST/PUT/DELETE
-- cuando tengamos las funciones de API en el lenguaje
registerRoute :: Server.ServerConfig.ServerConfig -> Route -> ScottyM ()
registerRoute config route =
    get (capture $ routePattern route) $ do
        params' <- params
        let urlParams = buildParams (routeSegments route) params'
        result <- liftIO $ runHsklFile config (routePath route) urlParams
        case result of
            Left  err  -> do
                status $ toEnum 500
                html $ TL.fromStrict $ T.pack $ errorPage err
            Right html' ->
                html $ TL.fromStrict $ T.pack html'

-- | Construye el mapa de parámetros URL
buildParams :: [Segment] -> [(Text, Text)] -> [(Text, Text)]
buildParams segments urlParams =
    [ (T.pack name, val)
    | (Dynamic name, _) <- zip segments (repeat ())
    , (key, val)        <- urlParams
    , T.pack (":" <> name) == key
    ]

-- ─── PÁGINA DE ERROR ─────────────────────────────────────────────────────────

errorPage :: String -> String
errorPage err = unlines
    [ "<!DOCTYPE html><html><head>"
    , "<title>HSKL Error</title>"
    , "<style>body{font-family:monospace;padding:2rem;background:#1a1a1a;color:#ff6b6b}"
    , "pre{background:#2a2a2a;padding:1rem;border-radius:4px;color:#ffd93d}</style>"
    , "</head><body>"
    , "<h1>💥 Error de HSKL</h1>"
    , "<pre>" ++ escapeHtml' err ++ "</pre>"
    , "</body></html>"
    ]

escapeHtml' :: String -> String
escapeHtml' = concatMap escape
  where
    escape '<'  = "&lt;"
    escape '>'  = "&gt;"
    escape '&'  = "&amp;"
    escape '"'  = "&quot;"
    escape c    = [c]