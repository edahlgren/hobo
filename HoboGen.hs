{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import           Data.Aeson (object)
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as B
import qualified Data.Attoparsec.Char8 as Atto
import qualified Data.HashMap.Strict as HM
import           Data.List (sort)
import           Web.Scotty
import           Network.Wai (Request(..))
import           Network.Wai.Middleware.Static
import           Network.Wai.Middleware.Rewrite
import           Network.Wai.Middleware.RequestLogger
import           Data.Monoid (mconcat, mappend)
import           Text.Markdown.Discount (markdown)
import		 System.IO (hPutStrLn, stderr)
import           System.FilePath ((</>))
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.Environment (getArgs)
import           Text.Hastache
import qualified Data.Text as T
import qualified Data.Text as LT
import           Data.Char (toLower, ord)
import           Data.Time.Format (formatTime, parseTime)
import           Data.Time.Calendar (fromGregorian, Day)
import           System.Locale (defaultTimeLocale)
import           Data.Maybe (catMaybes)
import qualified Data.List as L
import           Safe (fromJustNote)
import           System.Console.CmdArgs
import           Data.Word (Word8)
import		 Data.Cache.LRU.IO as LRU

markdownBS a = LB.fromChunks [markdown a]

markdownText = TLE.decodeUtf8

data TemplateSet = TemplateSet {
      postTemplate :: Template
    , homeTemplate :: Template
    , archiveTemplate :: Template
    , aboutTemplate :: Template
    }

type Template = (MuContext IO -> IO LB.ByteString)

data Config = Config {
      base :: String
    , port :: Int
    , preview :: Int
    } deriving (Show, Data, Typeable)

templateDir = "templates"

readTemplatesOrError base = do
    post <- B.readFile (base </> templateDir </> "post.html")
    home <- B.readFile (base </> templateDir </> "home.html")
    archive <- B.readFile (base </> templateDir </> "archive.html")
    about <- B.readFile (base </> templateDir </> "about.html")
    return $ TemplateSet
        (hastacheStr ourConfig post)
        (hastacheStr ourConfig home)
        (hastacheStr ourConfig archive)
        (hastacheStr ourConfig about)
  where
    ourConfig = defaultConfig {
                      muTemplateFileDir=Just includeDir
                    , muTemplateFileExt=Just ".html"
        }
    includeDir = base </> templateDir

data Post = Post (HM.HashMap B.ByteString B.ByteString) B.ByteString

parseHeaders :: Atto.Parser (HM.HashMap B.ByteString B.ByteString)
parseHeaders = do
    kvs <- Atto.many1 parseHeader
    Atto.skipSpace
    return $ HM.fromList kvs
  where
    parseHeader = do
        nm <- Atto.takeWhile (\c -> c `elem` ['a'..'z'] || c `elem` ['A'..'Z'])
        _ <- Atto.char ':'
        Atto.skipSpace
        val <- Atto.takeWhile (\c -> not $ c `elem` "\r\n")
        Atto.takeWhile (\c -> c `elem` "\r\n")
        return (B.map toLower nm, val)

parsePost :: B.ByteString -> Post
parsePost s =
    case Atto.parse parseHeaders s of
        Atto.Fail _ _ e -> error $ "parse error: " ++ e
        Atto.Partial _ -> error $ "parse error: only partial input while reading headers"
        Atto.Done left hs -> Post hs left

makeDate year month day = do
    fromGregorian iyear imonth iday
  where
    iyear = read year
    imonth = read month
    iday = read day

createDateDisplay day =
    formatTime defaultTimeLocale "%A, %B %e '%y" day

createTitleFromName = T.unpack . T.replace "-" " " . T.pack

rewritePostStatic :: Config -> [T.Text] -> a -> IO [T.Text]
rewritePostStatic cfg parts@("static":x) _ = return parts
rewritePostStatic cfg parts@(y:m:d:dn:"":[]) _ = return parts
rewritePostStatic cfg parts@(y:m:d:dn:fn:[]) _ = do
    case yearLike y of
        True -> do
            let nm = T.concat [y, "-", m, "-", d, "-", dn]
            return $ ["posts", nm, fn]
        False -> return parts
  where
    yearLike p = (T.length p == 4) && (all (\c-> c `elem` ['0'..'9']) $ T.unpack p)
rewritePostStatic cfg p _ = return p

makePostPath cfg year month day name file =
    let dn = mconcat [year, "-", month, "-", day, "-", name] in
    (base cfg) </> "posts" </> dn </> file

data PostFile = PostFile String Day String

parsePostName = parsePostParts . B.pack . (flip (++) "\n")

parseArchive = parseArchiveParts . B.pack . (flip (++) "\n")

parseArchiveParts f =
    case Atto.parse parseParts' f of
        Atto.Fail _ _ e -> Nothing
        Atto.Partial _ -> Nothing
        Atto.Done left (year, month, day, name@(n:_), url) ->
            if (n == '_') then Nothing else Just (year,month,name,url)

parsePostParts f =
    case Atto.parse parseFilename f of
        Atto.Fail _ _ e -> Nothing
        Atto.Partial _ -> Nothing
        Atto.Done left p@(PostFile _ _ (n:_)) ->
            if (n == '_') then Nothing else Just p

parseTitleParts = do
    year <- fmap B.unpack $ Atto.takeWhile (/= '-')
    Atto.char '-'
    month <- fmap B.unpack $ Atto.takeWhile (/= '-')
    Atto.char '-'
    day <- fmap B.unpack $ Atto.takeWhile (/= '-')
    Atto.char '-'
    name <- fmap B.unpack $ Atto.takeWhile (not . Atto.isEndOfLine . fromIntegral . ord)
    return (year, month, day, name)

parseParts' = do
    (year, month, day, name) <- parseTitleParts
    let date = makeDate year month day
    let url = concat ["./", year, "/", month, "/", day, "/", name, ".html"]
    return (year,month,date,name,url)

parseFilename = do
    (_,_,date,name,url) <- parseParts'
    return $ PostFile url date name

compilePages ts cfg = do
    rpaths <- getDirectoryContents (base cfg </> "posts")
    let valid = catMaybes $ map validatePath rpaths
    mapM logger valid
    pages <- mapM (compilePage ts cfg) valid
    return $ catMaybes pages
  where
    validatePath p = case parsePostName p of
        Just _ -> Just p
        Nothing -> Nothing

getPost cfg path = do
    let fullpath = (base cfg </> "posts" </> path </> "post.md")
    c <- B.readFile fullpath
    return $ parsePost c

compilePage ts cfg path = do
    (Post headers body) <- getPost cfg path
    let mod = markdownText . markdownBS $ body
    case parsePostName path of
        Just (PostFile _ dt _) -> do
            out <- (postTemplate ts) (sub headers mod dt)
            return $ Just (path,out)
        Nothing -> return Nothing
  where
    sub _ _ d "date" = MuVariable (createDateDisplay d)
    sub _ b _ "body" = MuVariable b
    sub h _ _ k =
        case HM.lookup k h of
            Just v -> MuVariable v
            Nothing -> MuNothing

compileHome ts cfg limit = do
    rpaths <- getDirectoryContents (base cfg </> "posts")
    let paths = (reverse . sort) rpaths
    let (allPosts, validPaths) = unzip . catMaybes $ map postWithPath paths
    texts <- mapM (bodyFromPost cfg limit) validPaths
    let postsWithText = zip allPosts texts
    out <- (homeTemplate ts) (sub postsWithText)
    return out
  where
    postWithPath path = case parsePostName path of
        Just post -> Just (post,path)
        Nothing -> Nothing

    previewText body (Just limit) = markdownText $ LB.intercalate "\n" previewLines
      where
        previewLines = take limit $ LB.split '\n' $ markdownBS body
    previewText body Nothing = markdownText . markdownBS $ body

    bodyFromPost cfg limit path =
      getPost cfg path >>= (\(Post _ body) -> return $ previewText body limit)

    sub posts "posts" = MuList $ map postCompleter posts
    sub posts o = MuNothing

    postCompleter ((PostFile u d n),_) "title" = MuVariable (createTitleFromName n)
    postCompleter ((PostFile u d n),_) "url" = MuVariable u
    postCompleter ((PostFile u d n),_) "date" = MuVariable (createDateDisplay d)
    postCompleter ((PostFile u d n),_) "name" = MuVariable n
    postCompleter (_,t) "text" = MuVariable t

compileArchive ts cfg = do
    rpaths <- getDirectoryContents (base cfg </> "posts")
    let paths = (reverse . sort) rpaths
    let archives = catMaybes $ map parseArchive paths
    let groupedArchives = L.groupBy (\(y1,m1,_,_) (y2,m2,_,_) -> y1 == y2 && m1 == m2) archives
    out <- (archiveTemplate ts) (sub groupedArchives)
    return out
  where
    sub posts "months" = MuList $ map monthCompleter posts
    sub posts o = MuNothing

    monthCompleter ((y,m,_,_):_) "month" = MuVariable (createYearMonth y m)
    monthCompleter titles "titles" = MuList $ map titleCompleter titles

    titleCompleter (_,_,title,_) "title" = MuVariable title
    titleCompleter (_,_,_,url) "url" = MuVariable url

createYearMonth :: String -> String -> String
createYearMonth y m = monthYear
  where monthYear = formatTime defaultTimeLocale "%B '%y" $ makeDate y m "0"

compileAbout ts cfg = (aboutTemplate ts) $ \_ -> MuNothing

writePage cfg text name = LB.writeFile (base cfg </> name ++ ".html") text

writeHome cfg text = writePage cfg text "index"

writePosts cfg pages = mapM_ (\(name,text) -> writePage cfg text name) pages

writeArchive cfg text = writePage cfg text "archive"

writeAbout cfg text = writePage cfg text "about"


data Sample = Sample {hello :: String}
              deriving (Show, Data, Typeable)

config = Config "." (-1)

compile cfg = do
    let limit = (if preview cfg == (-1) then Nothing else Just $ preview cfg)
    ts <- readTemplatesOrError (base cfg)
    home <- compileHome ts cfg limit
    writeHome cfg home
    posts <- compilePages ts cfg
    writePosts cfg posts
    archive <- compileArchive ts cfg
    writeArchive cfg archive
    about <- compileAbout ts cfg
    writeAbout cfg about

logger str = hPutStrLn stderr str

toLazyBS bstr = LB.fromChunks [bstr]

getPage cfg cache = do
    year <- param "year"
    month <- param "month"
    day <- param "day"
    name <- param "name"
    let fullpath = concat [(base cfg), "/", year, "-", month, "-", day, "-", name]
    liftIO $ logger ("fullpath=" ++ fullpath)
    exists <- liftIO $ doesFileExist fullpath
    case exists of
        True -> do
	    -- check the cache
	    maybePage <- liftIO $ LRU.lookup fullpath cache
	    case (maybePage) of
	        Just page -> html . TLE.decodeUtf8 $ page
		Nothing -> do
		    page <- liftIO $ B.readFile $ fullpath
		    let lazyPage = toLazyBS page
		    liftIO $ LRU.insert fullpath lazyPage cache
		    html . TLE.decodeUtf8 $ lazyPage
	False -> raise "Page not found"

loadBasicPage path cache = do
    exists <- doesFileExist path
    when (exists) $ do
        page <- B.readFile path
	let lazyPage = toLazyBS page
	LRU.insert path lazyPage cache

getBasicPage path cache = do
    maybePage <- liftIO $ LRU.lookup path cache
    case maybePage of
        Just page -> html . TLE.decodeUtf8 $ page
	Nothing -> raise "Page not found"

serve cfg = do
    -- pre cache the homepage
    -- otherwise use a simple lru for the pages, size very small.
    cache <- LRU.newAtomicLRU (Just 10)

    let homepath = (base cfg) ++ "/index.html"
    loadBasicPage homepath cache
    
    let aboutpath = (base cfg) ++ "/about.html"
    loadBasicPage aboutpath cache

    let archivepath = (base cfg) ++ "/archive.html"
    loadBasicPage archivepath cache

    scotty (port cfg) $ do
        middleware logStdoutDev
        middleware $ rewrite $ rewritePostStatic cfg	
        middleware $ staticPolicy
            (noDots >-> (hasPrefix "static/" <|> (hasPrefix "posts/" >-> contains "."))
            >-> addBase (base cfg))	
	get "/archive" $ do
	    getBasicPage archivepath cache
	get "/about" $ do
	    getBasicPage aboutpath cache
	get "/:year/:month/:day/:name" $ do
            getPage cfg cache
	get "/" $ do
	    getBasicPage homepath cache

getConfig (base:port:preview:[]) = Config base (read port) (read preview)
getConfig _ = error "wrong number of arguments to hobogen: use hobogen BASE_DIR PORT PREVIEW_LINES"

main = do
    args <- getArgs
    let cfg = getConfig args
    compile cfg
    serve cfg
