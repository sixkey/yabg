{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Data.Maybe
import Data.Bifunctor ( first, second )

import Control.Monad
import Control.Monad.Trans ( lift, liftIO, MonadIO )
import Control.Monad.Writer ( WriterT ( runWriterT ) )
import Control.Monad.Reader ( ReaderT (runReaderT), MonadReader, local, ask, asks )
import Control.Monad.RWS ( RWST, MonadWriter ( tell ), runRWST, get, tell, modify )

import System.FilePath.Find ( find, always, extension, (==?) )
import System.FilePath ( (</>), makeRelative, takeDirectory, (<.>), dropExtension, dropExtensions )
import System.Directory
import System.Directory.PathWalk
import System.Random ( RandomGen, initStdGen )
import System.Random.Shuffle ( shuffle' )

import Debug.Trace ( traceShowId )

import qualified Data.Text as DT ( lines, unlines, drop )
import Data.Text ( pack, unpack, Text, isPrefixOf, split )

import qualified Text.Pandoc as P
import Text.Pandoc.Shared ( trim )
import Text.Pandoc.Writers.Shared ( lookupMetaString )

import Text.Regex.PCRE

import Text.Blaze.Html.Renderer.String ( renderHtml )
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5 ( (!) )
import qualified Text.Blaze.Html5.Attributes as A

import Options.Applicative

-- Data -----------------------------------------------------------------------

data PostMeta = PostMeta { title :: String
                         , desc :: String
                         , image :: Maybe String
                         , scripts :: [ String ]
                         , postPath :: String } deriving Show

data Post = Post { meta :: PostMeta
                 , content :: H.Html }

data YabgSettings = YabgSettings { srcPath :: FilePath
                                 , dstPath :: FilePath
                                 , rootPath :: String
                                 , dev :: Bool
                                 , dirsToCopy :: [ ( FilePath, FilePath ) ]
                                 , defLinks :: [ String ]
                                 , nav :: [ ( String, String ) ] }

type BlockDirArgs = [ String ]

data YabgBlock = PDoc P.Pandoc
               | InlineDir [ String ]
               | BlockDir BlockDirArgs [ Text ] deriving Show

data YabgDoc = YabgDoc PostMeta [ YabgBlock ] deriving Show


data YabgContext = YabgContext { settings :: YabgSettings
                               , posts :: [ PostMeta ] }

newtype YabgContextT m a = YabgContextT { runContextT :: ReaderT YabgContext m a }
    deriving ( Functor, Applicative, Monad, MonadReader YabgContext, MonadIO )

type YabgMonadT = YabgContextT
type YabgMonad = YabgMonadT IO

runYabgMonad :: YabgMonad a -> YabgSettings -> [ PostMeta ] -> IO a
runYabgMonad mon stngs psts = runReaderT ( runContextT mon )
                                         ( YabgContext { settings = stngs
                                                       , posts = psts } )


-- Misc. IO -------------------------------------------------------------------

changeExt :: String -> FilePath -> FilePath
changeExt ext filepath = dropExtension filepath <.> ext

translatePath :: FilePath -> FilePath -> FilePath -> FilePath
translatePath srcDir dstDir path = dstDir </> makeRelative srcDir path

copyDir :: FilePath -> FilePath -> IO ()
copyDir srcDir dstDir = pathWalk srcDir $ \ dir subdirs files ->
    do createDirectoryIfMissing True ( tr dir )
       forM_ files $ \ file -> do
           let wholeFile = dir </> file
           copyFile wholeFile ( tr wholeFile )
    where
       tr = translatePath srcDir dstDir

-- Misc. ----------------------------------------------------------------------

getPandocMeta :: P.Pandoc -> P.Meta
getPandocMeta ( P.Pandoc meta blocks ) = meta;

post :: PostMeta -> H.Html -> Post
post meta content = Post { meta = meta
                         , content = content }


-- Path -----------------------------------------------------------------------

fromRoot :: YabgSettings -> String -> String
fromRoot sttngs = ( rootPath sttngs </> )

imagePath :: YabgSettings -> PostMeta -> Maybe String
imagePath sttngs meta = fromRoot sttngs <$> image meta

postUrl :: YabgSettings -> PostMeta -> String
postUrl sttngs pMeta = changeExt "html" $ fromRoot sttngs $ postPath pMeta

-- Generic Components ---------------------------------------------------------

maybeElement :: Maybe H.Html -> H.Html
maybeElement ( Just x ) = x
maybeElement Nothing = pure ()

-- Components -----------------------------------------------------------------

comment :: H.Html -> H.Html
comment = H.em ! A.class_ "comment"

postTitle :: String -> H.Html
postTitle = H.h1 . H.toHtml

postHeader :: Post -> H.Html
postHeader = postTitle . title . meta

sideBar :: YabgSettings -> Post -> Maybe H.Html
sideBar sttngs post = do
    imgPath <- imagePath sttngs ( meta post )
    return $ H.img ! ( A.src . H.stringValue $ imgPath )
                   ! A.id "title-image"

renderNavigation :: [ ( String, String ) ] -> H.Html
renderNavigation links = H.ul ! A.id "nav" $
    forM_ links $ \ ( text, url ) ->
        H.li $ H.a ! A.href ( H.stringValue url ) $ H.toHtml text

listLibrary :: YabgSettings -> [ PostMeta ] -> H.Html
listLibrary sttngs [] = comment "this library is empty"
listLibrary sttngs posts = H.div ! A.class_ "list-lib" $ mapM_ listPost posts
  where
    listPost :: PostMeta -> H.Html
    listPost post = H.div ! A.class_ "list-lib-tile" $
        H.a ! A.href ( H.stringValue ( postUrl sttngs post ) ) $ do
            H.p ! A.class_ "tile-title" $ H.toHtml ( title post )
            H.p ! A.class_ "tile-desc" $ H.toHtml ( desc post )

imageLibrary :: YabgSettings -> [ PostMeta ] -> H.Html
imageLibrary sttngs [] = comment "this library is empty"
imageLibrary sttngs posts = H.div ! A.class_ "image-lib" $ mapM_ imagePost posts
  where
    garImagePath :: PostMeta -> String
    garImagePath meta = fromMaybe ( fromRoot sttngs "public/square.png" )
                                  ( imagePath sttngs meta )
    imagePost :: PostMeta -> H.Html
    imagePost post = H.a ! A.href ( H.stringValue $ postUrl sttngs post )
                         ! A.class_ "image-lib-tile" $ do
        H.img ! A.src ( H.stringValue ( garImagePath post ) )
              ! A.class_ "image-lib-tile-bg"
        H.p ! A.class_ "image-lib-tile-fg"
            $ H.toHtml $ title post

divTable :: String -> [ String ] -> [ String ] -> [ String ] -> H.Html
divTable tableCls rows cols lns = H.div ! A.class_ ( H.stringValue tableCls ) $ go rows lns
  where
    c = length cols
    go :: [ String ] -> [ String ] -> H.Html
    go _ [] = pure ()
    go ( row : rowRest ) go_lines =
        let ( cur, nxt ) = splitAt c go_lines
         in do H.div ! A.class_ ( H.stringValue row ) $ do
                    mapM_ ( \( cls, l ) -> H.div ! A.class_ ( H.stringValue cls ) $ H.string l ) $ zip cols cur
               go ( rowRest ++ [ row ] ) nxt
    go _ _ = undefined

renderPost :: YabgSettings -> Post -> H.Html
renderPost settings post = H.html $ do
    H.head $ do
        H.title ( H.toHtml . title . meta $ post )
        H.script ! A.src ( H.stringValue $ unpack P.defaultMathJaxURL ) $ pure ()
        H.meta ! A.name ( H.stringValue $ "viewport" ) ! A.content ( H.stringValue "width=device-width, initial-scale=1.0" )
        when ( dev settings ) $
           H.script ! A.src ( H.stringValue "https://livejs.com/live.js" ) $ pure ()
        mapM_ ( \src -> H.script ! A.src ( H.stringValue ( fromRoot settings "public/scripts/" ++ src ++ ".js" ) ) 
                            $ pure () )
              ( scripts $ meta post )
        mapM_ ( \href -> H.link ! A.rel "stylesheet" ! A.href ( H.stringValue href ) )
              ( defLinks settings )
    H.body $ do
        H.div ! A.id "root" $ do
            H.div ! A.id "content-root" $ do
                H.div ! A.id "left-wing" $
                    maybeElement $ sideBar settings post
                H.div ! A.id "main-body" $ do
                    H.div ! A.id "title-nav" $ do
                        postHeader post
                        renderNavigation ( nav settings )
                    content post
            H.footer $ do 
                H.div ! A.class_ "footer-text" $ do 
                    H.div $ H.string "made with ❤️ in Brno and Leipzig"
                    H.div $ H.a ! A.href ( H.stringValue "https://github.com/sixkey/yabg" ) $ 
                            H.string "github"

-- Card --

cardNames = liftA2 (++)
                   [ "A", "2", "3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K" ]
                   [ "C", "S", "H", "D" ]


publicImage name stngs = H.stringValue $ fromRoot stngs ( "public/" ++ name )
           
renderCard :: YabgSettings -> String -> H.Html
renderCard stngs name = H.div ! A.class_ "card" $
                                do H.img ! A.class_ "card-front"
                                         ! A.src ( publicImage cardSrc stngs )
                                   H.img ! A.class_ "card-back" 
                                         ! A.src ( publicImage backSrc stngs )
    where 
        cardSrc = "cards/" ++ name ++ "@1x.png"
        backSrc = "cards/back.png"

renderDeck :: YabgSettings -> H.Html
renderDeck stngs = H.div ! A.id "deck" $ 
                mapM_ ( renderCard stngs ) cardNames

-- Pipelinining ---------------------------------------------------------------

yabgWriterOptions :: P.WriterOptions
yabgWriterOptions = P.def{ P.writerHTMLMathMethod = P.MathJax P.defaultMathJaxURL }

yabgReaderOptions :: P.ReaderOptions
yabgReaderOptions = P.def{ P.readerExtensions = P.pandocExtensions }

type ReadPostMonad = RWST () [ YabgBlock ] ( [ Text ], [ ( BlockDirArgs, [ Text ] ) ] ) YabgMonad

parseMarkdown :: Text -> YabgMonad P.Pandoc
parseMarkdown text = liftIO $ do
    P.runIOorExplode $ P.readMarkdown yabgReaderOptions text

postSrc :: PostMeta -> YabgMonad FilePath
postSrc postMeta = asks ( ( </> postPath postMeta ) . srcPath . settings )

onHead :: ( a -> a ) -> [ a ] -> [ a ]
onHead f ( x : xs ) = f x : xs
onHead f _ = undefined

readPost :: PostMeta -> YabgMonad YabgDoc
readPost postMeta = do
    pstSrc <- postSrc postMeta
    text <- liftIO $ readFile pstSrc
    goText ( pack text )
    where
        flushLines :: ReadPostMonad ()
        flushLines = do
            lns <- fst <$> get
            unless ( null lns ) $ do
                doc <- lift $ parseMarkdown ( DT.unlines . reverse $ lns )
                modify $ first $ const []
                tell . pure $ PDoc doc
        flushBlockDir :: ReadPostMonad ()
        flushBlockDir = do
            ( arguments, cont ) <- head . snd <$> get
            tell $ pure $ BlockDir arguments ( reverse cont )
            modify $ second tail
            return ()
        goText :: Text -> YabgMonad YabgDoc
        goText text = do
            ( _, _, blocks ) <- ( \x -> runRWST x () ( [], [] ) ) $ do
                mapM_ go ( DT.lines text )
                lns <- get
                flushLines
            return $ YabgDoc postMeta blocks
        go :: Text -> ReadPostMonad ()
        go line
            | "%%[" `isPrefixOf` line = do
               let cmnd = map ( unpack . trim ) $ split ( == ',' )
                                                ( trim . DT.drop 3 $ line )
               flushLines
               modify $ second ( ( cmnd, [] ) : )
               return ()
            | "%%]" `isPrefixOf` line = do
               flushBlockDir
               return ()
            | "%%%" `isPrefixOf` line = do

               let cmnd = map ( unpack . trim ) $ split ( == ',' )
                                                ( trim . DT.drop 3 $ line )
               flushLines
               tell $ pure $ InlineDir cmnd
               return ()
            | otherwise = do
               stck <- snd <$> get
               if null stck
                  then modify $ first ( line : )
                  else modify $ second $ onHead $ second ( line : )

strReplace og nw = map ( \e -> if e == og then nw else e )


documentToPost :: YabgDoc -> YabgMonad Post
documentToPost ( YabgDoc meta blocks ) = do
        ( _, html ) <- runWriterT $ forM_ blocks $ go
        return $ post meta html
    where
        filterAtom :: String -> String -> PostMeta -> Bool
        filterAtom "title" p post = title post =~ p
        filterAtom "path" p post = postPath post =~ p
        filterAtom t p _ = error $ "undefined yabg filter: '"
                                   ++ show t ++ " "
                                   ++ show p ++ "'"
        buildFilter :: [ String ] -> PostMeta -> Bool
        buildFilter [] pst = True
        buildFilter ( "not" : t : p : rest ) pst =
            not ( filterAtom t p pst ) && buildFilter rest pst
        buildFilter ( t : p : rest ) pst =
            filterAtom t p pst && buildFilter rest pst

        buildFilter x _ = error $ "undefined yabg filter: " ++ show x
        postList :: String
                 -> ( YabgSettings -> [ PostMeta ] -> H.Html )
                 -> [ String ]
                 -> WriterT H.Html YabgMonad ()
        postList listTitle listFun fltr = do
            psts <- asks posts
            sttngs <- asks settings
            let relevantPosts = filter ( buildFilter fltr ) psts
            tell ( H.h3 ! A.class_ "lib-title" $ H.toHtml listTitle )
            tell ( listFun sttngs relevantPosts )
        parseTitle = strReplace '_' ' '

        go ( InlineDir ( "image-library" : title : rest ) ) =
            postList ( parseTitle title ) imageLibrary rest
        go ( InlineDir ( "list-library" : title : rest ) ) =
            postList ( parseTitle title ) listLibrary rest
        go ( InlineDir [ "deck" ] ) =
            do 
                sttngs <- asks settings
                tell ( renderDeck sttngs )
        go ( InlineDir x ) = error $ "undefined yabg command: " ++ show x
        go ( BlockDir ( "div-table" : r : c : tableClass : classes ) text ) =
            let ( rows, cols ) = splitAt ( read r :: Int ) classes
             in do tell $ divTable tableClass rows cols ( map unpack text )
        go ( PDoc document ) =
            do html <- liftIO $ P.runIOorExplode $
                 P.writeHtml5 yabgWriterOptions document
               tell html

writePost :: FilePath -> Post -> YabgMonad ()
writePost path pst = do
    stngs <- asks settings
    let renderedHtml = renderPost stngs pst
    liftIO $ do
        createDirectoryIfMissing True $ takeDirectory path
        writeFile path ( renderHtml renderedHtml )

postPipeline :: PostMeta -> YabgMonad ()
postPipeline postMeta = do
    liftIO . print $ postPath postMeta
    stngs <- asks settings
    document <- readPost postMeta
    pst <- documentToPost document
    let outPath = changeExt "html" $ dstPath stngs </> postPath postMeta
    writePost outPath pst

-- This is just awful
readPostMeta :: FilePath -> FilePath -> YabgMonad PostMeta
readPostMeta basePath path = do
    content <- liftIO $ pack <$> readFile path
    meta <- liftIO $ P.runIOorExplode
        ( getPandocMeta <$> P.readMarkdown yabgReaderOptions content )
    return $ fromPandocMeta meta
   where
      fromPandocMeta :: P.Meta -> PostMeta
      fromPandocMeta pMeta = PostMeta { title = metaGet "title"
                                      , desc = metaGet "desc"
                                      , scripts = listify $ metaGet "scripts"
                                      , image = maybyfy $ metaGet "image"
                                      , postPath = makeRelative basePath path }
          where
              metaGet s = unpack $ lookupMetaString s pMeta
              maybyfy [] = Nothing
              maybyfy x = Just x
              listify [] = []
              listify s = [ s ]


yabgCopyDirs :: YabgMonad ()
yabgCopyDirs = do stngs <- asks settings
                  liftIO $ forM_ ( dirsToCopy stngs ) $ \ ( ogDir, nwDir ) -> do
                        copyDir ogDir ( dstPath stngs </> nwDir )

postDirectory :: YabgMonad ()
postDirectory =
    do src <- asks ( srcPath . settings )
       postNames <- liftIO $ find always ( extension ==? ".pst" ) src
       postMetas <- forM postNames ( readPostMeta src )
       local ( \x -> x{ posts = postMetas } ) $
           forM_ postMetas postPipeline

yabgPipeline :: YabgMonad ()
yabgPipeline = do
    yabgCopyDirs
    local ( \c -> let s = settings c
                   in c{ settings = s{ srcPath = srcPath s </> "pages"
                                     , dstPath = "bin" } } )
          postDirectory

---- Main ---------------------------------------------------------------------

data Argv = Argv
    { aRootPath :: String
    , aDev :: Bool
    }

argumentParser :: Parser Argv
argumentParser = Argv
    <$> strOption ( long "rootpath"
                 <> metavar "ROOTPATH"
                 <> help "The rootpath, e.g. / or /~xkucerak" )
    <*> switch
          ( long "dev"
         <> short 'd'
         <> help "whether to run in development" )

argumentParserInfo :: ParserInfo Argv
argumentParserInfo = info ( argumentParser <**> helper )
    ( fullDesc
    <> progDesc "Build a site using yet another blog generator." )

main :: IO ()
main = do print "yabg"
          args <- execParser argumentParserInfo
          let root = aRootPath args
          runYabgMonad yabgPipeline
                       YabgSettings { srcPath = "xlogin"
                                    , dstPath = "bin"
                                    , rootPath = root
                                    , dev = aDev args
                                    , dirsToCopy = [ ( "xlogin/public", "public" )
                                                   , ( "xlogin/data", "" ) ]
                                    , defLinks = [ root </> "public/bootstrap.min.css"
                                                 , root </> "public/index.css" ]
                                    , nav = [ ( "xkucerak", root </> "" )
                                            , ( "uni", root </> "uni" )
                                            , ( "blog", root </> "blog" )
                                            ] } []
